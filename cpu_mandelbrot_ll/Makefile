CC = clang++-37
MAKELL_SRC = create_ll.cpp

BIN_DIR = bin
OBJ_DIR = obj
MAKELL_OBJ = $(BIN_DIR)/$(MAKELL_SRC:.cpp=.o)

LLVM_CONFIG = llvm-config-37
LLVM_FLAGS = --cxxflags --ldflags --libs
FLAGS = -g
LIBS = -lncurses -lpthread -lz -lm -ldl 

LLI = lli-37
LL_NAME = mandelbrot.ll


all: $(MAKELL_SRC)
	mkdir -p $(BIN_DIR)
	mkdir -p $(OBJ_DIR)
	$(CC) $(FLAGS) $(MAKELL_SRC) -o $(MAKELL_OBJ)  `$(LLVM_CONFIG) $(LLVM_FLAGS)` $(LIBS) 
	$(MAKELL_OBJ) $(LL_NAME)

run:
	$(LLI) $(LL_NAME)

clean:
	rm $(RUNTIME_OBJ) $(MAKELL_OBJ) $(LL_NAME) $(LL_OBJ) $(BIN)
