
.PHONY: run clean

EXE := Main
SRCS := $(shell find . -iname '*.hs' -type f)

GRAMMAR := grammars/CEG-RR

run: $(EXE)
	./$(EXE) $(GRAMMAR)

$(EXE): $(SRCS)
	ghc Main.hs

clean:
	rm -f Code/*.o Code/*.hi $(EXE)