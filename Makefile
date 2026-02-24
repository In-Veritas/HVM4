C_SOURCES = $(shell find clang -name "*.c")
C_HEADERS = $(shell find clang -name "*.h")

main: clang

clang: $(C_SOURCES) $(C_HEADERS)
	clang -O2 -g -o main clang/main.c
	cp main clang/main

run: main
	./main

test: main
	./test/_all_.sh

clean:
	rm main *.hi *.o clang/main clang/*.o

.PHONY: clean run test clang
