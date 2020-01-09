#include <string>
#include <vector>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <cmath>
#include <sstream>
#include <memory>
#include <cstring>
#include <thread>
#include <chrono>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>

#define CL_HPP_ENABLE_EXCEPTIONS
#define CL_HPP_MINIMUM_OPENCL_VERSION 100
#define CL_HPP_TARGET_OPENCL_VERSION 120
#include <CL/cl2.hpp>

#include <cxxopts.hpp>
#include <json.hpp>

using byte = unsigned char;

enum class ArrayElementType {
  Int,
  Float,
};

ArrayElementType fromTypeStr(const std::string &str) {
  if (str == "int*") { return ArrayElementType::Int; }
  else if (str == "float*") { return ArrayElementType::Float; }
  else { throw "unknown type"; }
}

struct Array {
  std::unique_ptr<byte> data;
  const size_t elem_size;
  const size_t size;
  const ArrayElementType elem_type;

  Array(byte *d, size_t es, size_t s, ArrayElementType ty): data(d), elem_size(es), size(s), elem_type(ty) {}

  void dump(std::string sep, size_t s) {
    switch (elem_type) {
      case ArrayElementType::Int: {
                  std::vector<cl_int> vdata(s);
                  std::memcpy(vdata.data(), data.get(), elem_size * s);
                  for (auto &x : vdata) {
                    std::cout << x << sep;
                  }
                  return;
                }
      case ArrayElementType::Float: {
                  std::vector<cl_float> vdata(s);
                  std::memcpy(vdata.data(), data.get(), elem_size * s);
                  for (auto &x : vdata) {
                    std::cout << x << sep;
                  }
                  return;
                }
    }
  }

  size_t byte_size() const { return elem_size * size; }
};

int main(int argc, char *argv[])
{
  cxxopts::Options optp("runtime", "A runtime of min-lift");
  optp.add_options()
    ("k,kernel", "Kernel file name", cxxopts::value<std::string>())
    ("d,data", "Input data file(s) name", cxxopts::value<std::string>())
    ("q,quiet", "Be queit", cxxopts::value<bool>()->default_value("false"))
    ("c,check", "Only checks weather kernel can be compiled", cxxopts::value<bool>()->default_value("false"))
    ("p,profile", "Profiling mode", cxxopts::value<bool>()->default_value("false"));

  try {
    auto opts = optp.parse(argc, argv);
    const std::string file = opts["kernel"].as<std::string>();
    const bool quiet = opts["quiet"].count();
    const bool profile = opts["profile"].count();

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
    const int kernel_count = config["KernelCount"].get<int>();
    const auto input_types = config["InputTypes"];
    const auto result_type = fromTypeStr(config["ResultType"].get<std::string>());

    cl_context_properties properties[] = {
      CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0
    };
    cl::Context context(CL_DEVICE_TYPE_GPU, properties);

    auto devices = context.getInfo<CL_CONTEXT_DEVICES>();

    if (!quiet) {
      // std::cout << "Using device: " << devices[0].getInfo<CL_DEVICE_NAME>() << std::endl;
      // std::cout << "Chunk size: " << CHUNK_SIZE << std::endl;
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

    std::vector<Array> raw_xses;
    {
      std::vector<std::string> pathes;
      boost::algorithm::split(pathes, opts["data"].as<std::string>(), boost::is_any_of(","));

      int i = 0;
      for (auto &path : pathes) {
        std::ifstream ifs(path);

        auto input_type = fromTypeStr(input_types[i].get<std::string>());
        if (input_type == ArrayElementType::Int) {
          std::vector<cl_int> data;

          int v;
          while (ifs >> v) data.push_back(v);

          size_t elem_size = sizeof(cl_int);
          size_t size = data.size();
          byte* mem = new unsigned char[elem_size * size];

          std::memcpy(mem, data.data(), elem_size * size);

          raw_xses.emplace_back(mem, elem_size, size, input_type);
        }
        else if (input_type == ArrayElementType::Float) {
          std::vector<cl_float> data;

          cl_float v;
          while (ifs >> v) data.push_back(v);

          size_t elem_size = sizeof(cl_float);
          size_t size = data.size();
          byte* mem = new unsigned char[elem_size * size];

          std::memcpy(mem, data.data(), elem_size * size);

          raw_xses.emplace_back(mem, elem_size, size, input_type);
        }

        i++;
      }
    }
    const int N = raw_xses[0].size;

    if (!quiet && !profile) {
      std::cout << "input data" << std::endl;
      int i = 0;
      for (auto &xs : raw_xses) {
        std::cout << "  " << i << ": ";
        xs.dump(" ", xs.size);
        std::cout << std::endl;
        i++;
      }
    }

    cl::CommandQueue queue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);

    std::vector<cl::Buffer> xses;
    for (auto &xs : raw_xses) {
      xses.emplace_back(context, CL_MEM_READ_WRITE, xs.elem_size * xs.size);
      queue.enqueueWriteBuffer(xses[xses.size() - 1], CL_TRUE, 0, xs.byte_size(), reinterpret_cast<void*>(xs.data.get()));
    }

    size_t result_elem_size = 0;
    if (result_type == ArrayElementType::Int) result_elem_size = sizeof(cl_int);
    else if (result_type == ArrayElementType::Float) result_elem_size = sizeof(cl_float);
    Array raw_result(new byte[result_elem_size * raw_xses[0].size], result_elem_size, raw_xses[0].size, result_type);

    cl::Buffer result(context, CL_MEM_READ_WRITE, raw_result.byte_size());
    {
      std::vector<byte> zeros(raw_result.byte_size(), 0);
      queue.enqueueWriteBuffer(result, CL_TRUE, 0, raw_result.byte_size(), reinterpret_cast<void*>(zeros.data()));
    }
    cl::Buffer result_size(context, CL_MEM_READ_WRITE, sizeof(int));

    cl_ulong sum_exe_time = 0;
    // const int exe_count = profile ? 1000 : 1;
    const int exe_count = 1;

    auto get_exe_time = [](const cl::Event &event) {
      return event.getProfilingInfo<CL_PROFILING_COMMAND_END>() - event.getProfilingInfo<CL_PROFILING_COMMAND_START>();
    };

    for (int i = 0; i < exe_count; i++) {
      cl_ulong exe_time = 0;

      if (kernel_count >= 2) {
        cl::Kernel kernel(program, "KERNEL");
        cl::Kernel kernel2(program, "KERNEL2");

        std::unique_ptr<cl::Kernel> kernel3;
        if (kernel_count == 3) {
          kernel3 = std::make_unique<cl::Kernel>(program, "KERNEL3");
        }

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

          cl::Event event;
          queue.enqueueNDRangeKernel(
              kernel, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
          event.wait();

          exe_time = get_exe_time(event);
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

            cl::Event event;
            queue.enqueueNDRangeKernel(
                prefix_sum, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
            event.wait();
            exe_time += get_exe_time(event);
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

          cl::Event event;
          queue.enqueueNDRangeKernel(
              kernel2, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
          event.wait();

          exe_time += get_exe_time(event);
        }

        if (kernel_count == 3) {
          int raw_result_size = 0;
          queue.enqueueReadBuffer(result_size, CL_TRUE, 0, sizeof(int), reinterpret_cast<void*>(&raw_result_size));

          // TODO: fix size gap
          queue.enqueueCopyBuffer(result, xses[0], 0, 0, raw_result.byte_size());
          int arg_i = 0;
          for (auto &xs : xses) {
            kernel3->setArg(arg_i, xs);
            arg_i++;
          }
          kernel3->setArg(arg_i++, result);
          kernel3->setArg(arg_i++, result_size);
          cl_int cl_N = raw_result_size;
          kernel3->setArg(arg_i, sizeof(cl_N), &cl_N);

          cl::Event event;
          queue.enqueueNDRangeKernel(
              *kernel3.get(), cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
          event.wait();

          exe_time += get_exe_time(event);
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

        cl::Event event;
        queue.enqueueNDRangeKernel(
            kernel, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
        event.wait();

        exe_time = get_exe_time(event);
      }

      sum_exe_time += exe_time;
      // if (!quiet) {
      //   std::cout << "interval execution time: " << static_cast<double>(exe_time) * 1e-3f << " us" << std::endl;
      // }

      // std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    int raw_result_size = 0;
    queue.enqueueReadBuffer(result_size, CL_TRUE, 0, sizeof(int), reinterpret_cast<void*>(&raw_result_size));

    queue.enqueueReadBuffer(result, CL_TRUE, 0, raw_result.elem_size * raw_result_size, reinterpret_cast<void*>(raw_result.data.get()));

    if (!quiet && !profile) {
      std::cout << "result size: " << raw_result_size << std::endl;
    }
    if (!profile) {
      raw_result.dump("\n", raw_result_size);
    }

    if (!quiet) {
      // std::cout << "execution time: " << static_cast<double>(sum_exe_time) / exe_count * 1e-3f << " us" << std::endl;
      std::cout << static_cast<double>(sum_exe_time) / exe_count * 1e-3f << std::endl;
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
