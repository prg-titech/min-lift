#include <string>
#include <vector>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <cmath>

#define CL_HPP_ENABLE_EXCEPTIONS
#define CL_HPP_MINIMUM_OPENCL_VERSION 100
#define CL_HPP_TARGET_OPENCL_VERSION 120
#include <CL/cl2.hpp>

#include <cxxopts.hpp>

const int CHUNK_SIZE = 5;

int main(int argc, char *argv[])
{
  cxxopts::Options optp("runtime", "A runtime of min-lift");
  optp.add_options()
    ("f,file", "Kernel file name", cxxopts::value<std::string>())
    ("d,data", "Input data file name", cxxopts::value<std::string>())
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

    cl_context_properties properties[] = {
      CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0
    };
    cl::Context context(CL_DEVICE_TYPE_GPU, properties);

    auto devices = context.getInfo<CL_CONTEXT_DEVICES>();

    if (!quiet) {
      std::cout << "Using device: " << devices[0].getInfo<CL_DEVICE_NAME>() << std::endl;
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

    cl::Kernel kernel(program, "KERNEL");

    if (opts.count("check")) {
      return 0;
    }

    std::vector<float> raw_xs;
    {
      std::ifstream ifs(opts["data"].as<std::string>());
      float v;
      while (ifs >> v) raw_xs.push_back(v);
    }
    const int N = raw_xs.size();

    cl::Buffer xs(context, CL_MEM_READ_WRITE, sizeof(float) * N);
    cl::Buffer result(context, CL_MEM_READ_WRITE, sizeof(float) * N);

    kernel.setArg(0, xs);
    kernel.setArg(1, result);
    cl_int cl_N = N;
    kernel.setArg(2, sizeof(cl_N), &cl_N);

    cl::Event event;
    cl::CommandQueue queue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);

    queue.enqueueWriteBuffer(xs, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_xs.data()));

    queue.enqueueNDRangeKernel(
        kernel, cl::NullRange, cl::NDRange(std::ceil(N/* / static_cast<float>(CHUNK_SIZE)*/)), cl::NullRange, nullptr, &event);
    event.wait();

    std::vector<float> raw_result(raw_xs.size());
    queue.enqueueReadBuffer(result, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_result.data()));

    if (!quiet) {
      std::cout << "raw_result = ";
      for (auto v : raw_result) {
        std::cout << v << ", ";
      }
      std::cout << std::endl;

      cl_ulong start = event.getProfilingInfo<CL_PROFILING_COMMAND_START>();
      cl_ulong end = event.getProfilingInfo<CL_PROFILING_COMMAND_END>();
      std::cout << "execution time: " << static_cast<double>(end-start)*1e-3f << " us" << std::endl;
    }

    return 0;
  } catch (cl::Error const& ex) {
    std::cerr << "OpenCL Error: " << ex.what() << " (code " << ex.err() << ")" << std::endl;
    return 1;
  } catch (cxxopts::OptionException& e) {
    std::cout << e.what() << std::endl;
    std::cout << optp.help() << std::endl;
    return 1;
  } catch (std::exception const& ex) {
    std::cerr << "Exception: " << ex.what() << std::endl;
    return 1;
  }
}
