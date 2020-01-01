exe:
	clang++ --std=c++14 -g -O3 toy.cpp `/media/veracrypt1/bin/llvm-project/build/bin/llvm-config --cxxflags --ldflags --libs --libfiles` -o exe
