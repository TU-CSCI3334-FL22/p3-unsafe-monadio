
.PHONY: run clean

EXE := Main
SRCS := $(shell find . -iname '*.hs' -type f)

GRAMMAR := grammars/SN-nonLL1-RR

run: $(EXE)
	./$(EXE) -wt $(GRAMMAR)

$(EXE): $(SRCS)
	ghc Main.hs -o $(EXE)

clean:
	rm -f Code/*.o Code/*.hi $(EXE)