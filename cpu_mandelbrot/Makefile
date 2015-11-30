CC = clang++-37
MAKELL_SRC = mandelbrot.cpp

BIN_DIR = bin
BIN = $(BIN_DIR)/$(MAKELL_SRC:.cpp=.o)

FLAGS = -g


all: $(MAKELL_SRC)
	mkdir -p $(BIN_DIR)
	$(CC) $(FLAGS) $(MAKELL_SRC) -o $(BIN)

run:
	$(BIN)

clean:
	rm $(BIN)
