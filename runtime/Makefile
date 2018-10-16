# COMPILER = sudo nvidia-docker run --rm -v `pwd`:/min-lift -it min-lift g++ -std=c++11
COMPILER = clang++ -std=c++11

all: runtime profiler

ext-include/cxxopts.hpp:
	curl https://raw.githubusercontent.com/long-long-float/cxxopts/add-error-handling-with-not-providing-arg/include/cxxopts.hpp > ext-include/cxxopts.hpp

ext-include/json.hpp:
	curl https://raw.githubusercontent.com/nlohmann/json/master/single_include/nlohmann/json.hpp > ext-include/json.hpp

runtime: runtime.cpp ext-include/cxxopts.hpp ext-include/json.hpp
	$(COMPILER) runtime.cpp -I ext-include -lOpenCL -o runtime

profiler: profiler.cpp
	$(COMPILER) profiler.cpp -lOpenCL -o profiler

test: runtime
	@sh run-artifacts.sh ../examples

.PHONY: clean
clean:
	rm -f runtime profiler
