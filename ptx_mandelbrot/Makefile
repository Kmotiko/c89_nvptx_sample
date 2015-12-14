CC = clang++-37
MAKELL_SRC = create_kernel.cpp

BIN_DIR = bin
OBJ_DIR = obj
MAKELL_OBJ = $(BIN_DIR)/$(MAKELL_SRC:.cpp=.o)

HOST_SRC = host.cpp
CUDA_INC_FLAGS = -I/usr/local/cuda-7.5/include
CUDA_LIBS = -lcuda
CUDA_FLAGS = -O2 -g $(CUDA_INC_FLAGS) $(CUDA_LIBS)
BIN = $(BIN_DIR)/$(HOST_SRC:.cpp=.o)


LLVM_CONFIG = llvm-config-37
LLVM_FLAGS = --cxxflags --ldflags --libs
FLAGS = -g
LIBS = -lncurses -lpthread -lz -lm -ldl 

LLC = llc-37
LL_FLAGS = -march=nvptx -mcpu=sm_30
LL_NAME = kernel.ll
KERNEL_OBJ = $(OBJ_DIR)/$(LL_NAME:.ll=.ptx)


all: $(MAKELL_SRC)
	mkdir -p $(BIN_DIR)
	mkdir -p $(OBJ_DIR)
	$(CC) $(FLAGS) $(MAKELL_SRC) -o $(MAKELL_OBJ)  `$(LLVM_CONFIG) $(LLVM_FLAGS)` $(LIBS) 
	$(MAKELL_OBJ) $(LL_NAME)
	$(LLC) $(LL_NAME) $(LL_FLAGS) -o $(KERNEL_OBJ)
#	$(CC) $(HOST_SRC) -o $(BIN) $(CUDA_FLAGS)

run:
	$(BIN)

clean:
	rm $(MAKELL_OBJ) $(LL_NAME) $(KERNEL_OBJ) $(BIN)
