
.PHONY: run clean

EXE := Main
SRCS := $(shell find . -iname '*.hs' -type f)

run: $(EXE)
	./$(EXE) grammars/CEG-RR

$(EXE): $(SRCS)
	ghc Main.hs

clean:
	rm Code/*.o Code/*.hi