exe: toy.cpp
	clang++ --std=c++14 -g -pthread toy.cpp `/media/veracrypt1/bin/llvm-project/build/bin/llvm-config --cxxflags --ldflags --libs core mcjit native --libfiles` -O3 -ltinfo -o exe
