# COMPILER = sudo nvidia-docker run --rm -v `pwd`:/min-lift -it min-lift g++ -std=c++11
COMPILER = g++ -std=c++11

all: runtime profiler

runtime: runtime.cpp
	$(COMPILER) runtime.cpp -lOpenCL -o runtime

profiler: profiler.cpp
	$(COMPILER) profiler.cpp -lOpenCL -o profiler

test: runtime
	@sh run-artifacts.sh ../examples

.PHONY: clean
clean:
	rm -f runtime profiler
