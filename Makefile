COMPILER = sudo nvidia-docker run --rm -v `pwd`:/min-lift -it min-lift g++ -std=c++11

all: runtime profiler

runtime: runtime.cpp
	$(COMPILER) /min-lift/runtime.cpp -lOpenCL -o /min-lift/runtime

profiler: profiler.cpp
	$(COMPILER) /min-lift/profiler.cpp -lOpenCL -o /min-lift/profiler

.PHONY: clean
clean:
	rm -f runtime profiler
