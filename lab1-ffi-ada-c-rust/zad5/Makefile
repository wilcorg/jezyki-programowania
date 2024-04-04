CFLAGS = -std=c17 -O3 -Wall -Wextra -Wpedantic -Wstrict-aliasing
CFLAGS += -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align
CFLAGS += -Wstrict-prototypes -Wstrict-overflow=5 -Wwrite-strings
CFLAGS += -Wcast-qual -Wswitch-default -Wswitch-enum
CFLAGS += -Wconversion -Wunreachable-code
CFLAGS += -Ic

BIN = z5
BUILD_DIR = target

.PHONY: clean build

build: dirs
	gcc -c c/lib/iter.c -o $(BUILD_DIR)/c/iter.o $(CFLAGS)
	gcc -c c/lib/rec.c -o $(BUILD_DIR)/c/rec.o $(CFLAGS)
	gnatmake -c main.adb -D $(BUILD_DIR)/ada/ -Iutils
	gnatbind -x $(BUILD_DIR)/ada/main.ali
	gnatlink $(BUILD_DIR)/ada/main.ali $(BUILD_DIR)/c/iter.o $(BUILD_DIR)/c/rec.o -o $(BUILD_DIR)/$(BIN)

run: build
	./$(BUILD_DIR)/$(BIN)

dirs:
	mkdir -p $(BUILD_DIR)/ada $(BUILD_DIR)/c

clean:
	rm -rf $(BUILD_DIR)
