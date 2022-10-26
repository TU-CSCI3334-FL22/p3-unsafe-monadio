
.PHONY: run clean

EXE := Main
SRCS := $(shell find . -iname '*.hs' -type f)

GRAMMAR := grammars/CEG-RR

run: $(EXE)
	./$(EXE) -t $(GRAMMAR)

$(EXE): $(SRCS)
	ghc Main.hs -o $(EXE)

clean:
	rm -f Code/*.o Code/*.hi $(EXE)