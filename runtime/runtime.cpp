#include <string>
#include <vector>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <cmath>
#include <sstream>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>

#define CL_HPP_ENABLE_EXCEPTIONS
#define CL_HPP_MINIMUM_OPENCL_VERSION 100
#define CL_HPP_TARGET_OPENCL_VERSION 120
#include <CL/cl2.hpp>

#include <cxxopts.hpp>
#include <json.hpp>

int main(int argc, char *argv[])
{
  cxxopts::Options optp("runtime", "A runtime of min-lift");
  optp.add_options()
    ("f,file", "Kernel file name", cxxopts::value<std::string>())
    ("d,data", "Input data file(s) name", cxxopts::value<std::string>())
    ("q,quiet", "Be queit", cxxopts::value<bool>()->default_value("false"))
    ("c,check", "Only checks weather kernel can be compiled", cxxopts::value<bool>()->default_value("false"));

  try {
    auto opts = optp.parse(argc, argv);
    const std::string file = opts["file"].as<std::string>();
    const bool quiet = opts["quiet"].count();

    std::ifstream ifs(file);
    if (ifs.fail()) {
      std::cerr << "Cannot read '" << file << "'" << std::endl;
      return 1;
    }

    std::string code = std::string(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
    std::vector<cl::Platform> platforms;
    cl::Platform::get(&platforms);
    if (platforms.size() == 0) {
      std::cerr << "No platform found." << std::endl;
      return 1;
    }

    // read config
    std::string config_json;
    {
      std::stringstream ss(code);
      std::string first_line;
      getline(ss, first_line, '\n');

      config_json = first_line.substr(3);
    }
    auto config = nlohmann::json::parse(config_json);
    const int CHUNK_SIZE = config["ChunkSize"].get<int>();
    const bool use_filter_global = config["UseFilterGlobal"].get<bool>();

    cl_context_properties properties[] = {
      CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0
    };
    cl::Context context(CL_DEVICE_TYPE_GPU, properties);

    auto devices = context.getInfo<CL_CONTEXT_DEVICES>();

    if (!quiet) {
      std::cout << "Using device: " << devices[0].getInfo<CL_DEVICE_NAME>() << std::endl;
      std::cout << "Chunk size: " << CHUNK_SIZE << std::endl;
    }

    cl::Program program(context, code);
    try {
      program.build({devices[0]});
    } catch (cl::Error const& ex) {
      if (ex.err() == CL_BUILD_PROGRAM_FAILURE) {
        auto buildlog = program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devices[0]);
        std::cerr << "Build Error: " << buildlog << std::endl;
        return 1;
      }
      else {
        throw ex;
      }
    }

    if (opts.count("check")) {
      return 0;
    }

    std::vector<std::vector<float>> raw_xses;
    {
      std::vector<std::string> pathes;
      boost::algorithm::split(pathes, opts["data"].as<std::string>(), boost::is_any_of(","));
      for (auto &path : pathes) {
        raw_xses.emplace_back();
        auto &raw_xs = raw_xses[raw_xses.size() - 1];

        std::ifstream ifs(path);
        float v;
        while (ifs >> v) raw_xs.push_back(v);
      }
    }
    const int N = raw_xses[0].size();

    if (!quiet) {
      std::cout << "input data" << std::endl;
      int i = 0;
      for (auto &xs : raw_xses) {
        std::cout << "  " << i << ": ";
        for (auto &x : xs) {
          std::cout << x << " ";
        }
        std::cout << std::endl;
        i++;
      }
    }

    std::vector<cl::Buffer> xses;
    for (auto &xs : raw_xses) {
      xses.emplace_back(context, CL_MEM_READ_WRITE, sizeof(float) * N);
    }
    cl::Buffer result(context, CL_MEM_READ_WRITE, sizeof(float) * N);

    cl::Buffer result_size(context, CL_MEM_READ_WRITE, sizeof(int));

    cl::Event event;
    cl::CommandQueue queue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);

    for (int i = 0; i < raw_xses.size(); i++) {
      queue.enqueueWriteBuffer(xses[i], CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_xses[i].data()));
    }
    {
      std::vector<float> zeros(N, 0.0f);
      queue.enqueueWriteBuffer(result, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(zeros.data()));
    }

    if (use_filter_global) {
      cl::Kernel kernel(program, "KERNEL");
      cl::Kernel kernel2(program, "KERNEL2");
      cl::Kernel prefix_sum(program, "prefix_sum");

      const cl::size_type bitmap_size = sizeof(int) * N;
      cl::Buffer bitmap(context, CL_MEM_READ_WRITE, bitmap_size);
      cl::Buffer bitmap_dup(context, CL_MEM_READ_WRITE, bitmap_size);

      cl::Buffer indices(context, CL_MEM_READ_WRITE, bitmap_size);

      {
        int arg_i = 0;
        for (auto &xs : xses) {
          kernel.setArg(arg_i, xs);
          arg_i++;
        }
        kernel.setArg(arg_i++, result);
        kernel.setArg(arg_i++, result_size);
        kernel.setArg(arg_i++, bitmap);
        cl_int cl_N = N;
        kernel.setArg(arg_i, sizeof(cl_N), &cl_N);

        queue.enqueueNDRangeKernel(
            kernel, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
        event.wait();
      }

      {
        queue.enqueueCopyBuffer(bitmap, bitmap_dup, 0, 0, bitmap_size);

        // FIXME: also supports dynamic size
        const int steps = std::ceil(std::log2((float)N));
        for (cl_int i = 0; i < steps; i++) {
          int arg_i = 0;
          prefix_sum.setArg(arg_i++, bitmap_dup);
          prefix_sum.setArg(arg_i++, indices);
          prefix_sum.setArg(arg_i++, i);

          queue.enqueueNDRangeKernel(
              prefix_sum, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
          event.wait();
        }
        if (steps % 2 == 0) {
          queue.enqueueCopyBuffer(bitmap_dup, indices, 0, 0, bitmap_size);
        }
      }

      {
        int arg_i = 0;
        for (auto &xs : xses) {
          kernel2.setArg(arg_i, xs);
          arg_i++;
        }
        kernel2.setArg(arg_i++, result);
        kernel2.setArg(arg_i++, result_size);
        kernel2.setArg(arg_i++, bitmap);
        kernel2.setArg(arg_i++, indices);
        cl_int cl_N = N;
        kernel2.setArg(arg_i, sizeof(cl_N), &cl_N);

        queue.enqueueNDRangeKernel(
            kernel2, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
        event.wait();
      }
    }
    else {
      cl::Kernel kernel(program, "KERNEL");

      int arg_i = 0;
      for (auto &xs : xses) {
        kernel.setArg(arg_i, xs);
        arg_i++;
      }
      kernel.setArg(arg_i++, result);
      kernel.setArg(arg_i++, result_size);
      cl_int cl_N = N;
      kernel.setArg(arg_i, sizeof(cl_N), &cl_N);

      queue.enqueueNDRangeKernel(
          kernel, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
      event.wait();
    }

    std::vector<float> raw_result(raw_xses[0].size());
    queue.enqueueReadBuffer(result, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_result.data()));

    int raw_result_size = 0;
    queue.enqueueReadBuffer(result_size, CL_TRUE, 0, sizeof(int), reinterpret_cast<void*>(&raw_result_size));

    if (!quiet) {
      std::cout << "result size: " << raw_result_size << std::endl;
    }
    for (auto v : raw_result) {
      std::cout << v << std::endl;
    }

    if (!quiet) {
      cl_ulong start = event.getProfilingInfo<CL_PROFILING_COMMAND_START>();
      cl_ulong end = event.getProfilingInfo<CL_PROFILING_COMMAND_END>();
      std::cout << "execution time: " << static_cast<double>(end-start)*1e-3f << " us" << std::endl;
    }

    return 0;
  } catch (cl::Error const& ex) {
    std::cerr << "OpenCL Error: " << ex.what() << " (code " << ex.err() << ")" << std::endl;
    return 1;
  } catch (std::domain_error& e) {
    std::cout << optp.help() << std::endl;
    return 1;
  } catch (std::exception const& ex) {
    std::cerr << "Exception: " << ex.what() << std::endl;
    return 1;
  }
}
