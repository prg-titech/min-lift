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

const int CHUNK_SIZE = 5;

std::string read_file(const std::string &path) {
  std::ifstream ifs(path);
  if (ifs.fail()) {
    throw "Cannot read '" + path + "'";
  }

  return std::string(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
}

int main(int argc, char *argv[])
{
  if (argc != 4) {
    std::cerr << "Usage: " << argv[0] << " FILE REF_FILE INPUT_DATA" << std::endl;
    return 1;
  }

  auto code = read_file(argv[1]);
  auto ref_code = read_file(argv[2]);

  try {
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

    std::cout << "Using device: " << devices[0].getInfo<CL_DEVICE_NAME>() << std::endl;

    std::vector<float> raw_xs;
    {
      std::ifstream ifs(argv[3]);
      float v;
      while (ifs >> v) raw_xs.push_back(v);
    }
    const int N = raw_xs.size();

    // compile and run code
    std::vector<float> raw_result(N);
    {
      cl::Program program(context, code);
      program.build({devices[0]});

      cl::Kernel kernel(program, "KERNEL");

      cl::Buffer xs(context, CL_MEM_READ_WRITE, sizeof(float) * N);
      cl::Buffer result(context, CL_MEM_READ_WRITE, sizeof(float) * N);

      kernel.setArg(0, xs);
      kernel.setArg(1, result);
      cl_int cl_N = N;
      kernel.setArg(2, sizeof(cl_N), &cl_N);

      cl::Event event;
      cl::CommandQueue queue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);

      queue.enqueueWriteBuffer(xs, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_xs.data()));

      const int LOOP_NUM = 1;
      cl_ulong sum_time = 0;
      for (int i = 0; i < LOOP_NUM; i++) {
        queue.enqueueNDRangeKernel(
            kernel, cl::NullRange, cl::NDRange(std::ceil(N / static_cast<float>(CHUNK_SIZE))), cl::NullRange, nullptr, &event);
        event.wait();

        cl_ulong start = event.getProfilingInfo<CL_PROFILING_COMMAND_START>();
        cl_ulong end = event.getProfilingInfo<CL_PROFILING_COMMAND_END>();
        sum_time += end - start;
      }
      std::cout << "execution time: " << static_cast<double>(sum_time) / LOOP_NUM * 1e-3f << " us" << std::endl;

      queue.enqueueReadBuffer(result, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_result.data()));
    }

    // compile and run ref_code
    std::vector<float> raw_ref_result(N);
    {
      cl::Program program(context, ref_code);
      program.build({devices[0]});

      cl::Kernel kernel(program, "KERNEL");

      cl::Buffer xs(context, CL_MEM_READ_WRITE, sizeof(float) * N);
      cl::Buffer result(context, CL_MEM_READ_WRITE, sizeof(float) * N);

      kernel.setArg(0, xs);
      kernel.setArg(1, result);
      cl_int cl_N = N;
      kernel.setArg(2, sizeof(cl_N), &cl_N);

      cl::Event event;
      cl::CommandQueue queue(context, devices[0]);

      queue.enqueueWriteBuffer(xs, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_xs.data()));

      queue.enqueueNDRangeKernel(
          kernel, cl::NullRange, cl::NDRange(std::ceil(N / static_cast<float>(CHUNK_SIZE))), cl::NullRange, nullptr, &event);
      event.wait();

      queue.enqueueReadBuffer(result, CL_TRUE, 0, sizeof(float) * N, reinterpret_cast<void*>(raw_ref_result.data()));
    }

    int accurate_count = 0;
    for (int i = 0; i < raw_result.size(); i++) {
      // std::cout << raw_result[i] << ", " <<  raw_ref_result[i]<< std::endl;
      if (std::abs(raw_result[i] - raw_ref_result[i]) <= std::numeric_limits<float>::epsilon()) {
        accurate_count++;
      }
    }
    std::cout << "accuracy: " << accurate_count / static_cast<float>(raw_result.size()) << std::endl;

    return 0;
  } catch (cl::Error const& ex) {
    std::cerr << "OpenCL Error: " << ex.what() << " (code " << ex.err() << ")" << std::endl;
    return 1;
  } catch (std::exception const& ex) {
    std::cerr << "Exception: " << ex.what() << std::endl;
    return 1;
  }
}
