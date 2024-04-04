CFLAGS = -std=c17 -O3 -Wall -Wextra -Wpedantic -Wstrict-aliasing
CFLAGS += -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align
CFLAGS += -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings
CFLAGS += -Wcast-qual -Wswitch-default -Wswitch-enum
CFLAGS += -Wconversion -Wunreachable-code
CFLAGS += -Ic

BIN = z7
BUILD_DIR = target

.PHONY: clean build

build:
	cargo build
	gcc -o $(BUILD_DIR)/debug/$(BIN) src/main.c $(CFLAGS) -L. -l:target/debug/libz7.so

run: build
	./$(BUILD_DIR)/debug/$(BIN)

clean:
	rm -rf $(BUILD_DIR)