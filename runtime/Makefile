# COMPILER = sudo nvidia-docker run --rm -v `pwd`:/min-lift -it min-lift g++ -std=c++11
COMPILER = g++ -std=c++11

all: runtime profiler

ext-include/cxxopts.hpp:
	curl https://raw.githubusercontent.com/long-long-float/cxxopts/add-error-handling-with-not-providing-arg/include/cxxopts.hpp > ext-include/cxxopts.hpp

runtime: runtime.cpp ext-include/cxxopts.hpp
	$(COMPILER) runtime.cpp -I ext-include -lOpenCL -o runtime

profiler: profiler.cpp
	$(COMPILER) profiler.cpp -lOpenCL -o profiler

test: runtime
	@sh run-artifacts.sh ../examples

.PHONY: clean
clean:
	rm -f runtime profiler
