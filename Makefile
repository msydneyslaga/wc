BUILD_DIR := build

all: $(BUILD_DIR) wc

$(BUILD_DIR):
	mkdir $(BUILD_DIR) || true

wc: src/wc.hs
	ghc -package utf8-string -outputdir $(BUILD_DIR) -dynamic $< -o $@
