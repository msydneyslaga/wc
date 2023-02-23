BUILD_DIR := build

all: $(BUILD_DIR) $(BUILD_DIR)/wc $(BUILD_DIR)/wc.pdf

$(BUILD_DIR):
	mkdir $(BUILD_DIR) || true

$(BUILD_DIR)/wc: src/wc.lhs
	ghc -package utf8-string -outputdir $(BUILD_DIR) -dynamic $< -o $@

$(BUILD_DIR)/wc.pdf: src/wc.lhs
	xelatex -shell-escape -output-directory=build $<

.PHONY:
clean:
	rm -rf $(BUILD_DIR)
