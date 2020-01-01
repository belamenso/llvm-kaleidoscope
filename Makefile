exe: toy.cpp
	clang++ --std=c++14 -g -O3 -pthread toy.cpp `/media/veracrypt1/bin/llvm-project/build/bin/llvm-config --cxxflags --ldflags --libs --libfiles` -ltinfo -o exe
