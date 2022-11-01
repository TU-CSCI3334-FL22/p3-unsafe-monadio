
.PHONY: run clean

EXE := llgen
SRCS := $(shell find . -iname '*.hs' -type f)

GRAMMAR := grammars/SN-nonLL1-RR

build: $(EXE)

run: $(EXE)
	./$(EXE) -wt $(GRAMMAR)

$(EXE): $(SRCS)
	ghc Main.hs -o $(EXE)

clean:
	rm -f Code/*.o Code/*.hi $(EXE)