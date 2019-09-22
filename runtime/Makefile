# COMPILER = sudo nvidia-docker run --rm -v `pwd`:/min-lift -it min-lift g++ -std=c++11
COMPILER = clang++ -std=c++14

all: runtime profiler prime

ext-include/cxxopts.hpp:
	curl https://raw.githubusercontent.com/jarro2783/cxxopts/master/include/cxxopts.hpp > ext-include/cxxopts.hpp

ext-include/json.hpp:
	curl https://raw.githubusercontent.com/nlohmann/json/master/single_include/nlohmann/json.hpp > ext-include/json.hpp

runtime: runtime.cpp ext-include/cxxopts.hpp ext-include/json.hpp
	$(COMPILER) runtime.cpp -I ext-include -lOpenCL -o runtime

prime: prime.cpp ext-include/cxxopts.hpp ext-include/json.hpp
	$(COMPILER) prime.cpp -I ext-include -lOpenCL -o prime

profiler: profiler.cpp
	$(COMPILER) profiler.cpp -lOpenCL -o profiler

test: runtime
	@sh run-artifacts.sh ../examples

clean-cl:
	rm ../examples/*.cl

.PHONY: clean
clean:
	rm -f runtime profiler
