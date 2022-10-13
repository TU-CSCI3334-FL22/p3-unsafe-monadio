
EXE := Main
SRCS := $(shell find . -iname '*.hs' -type f)

run: $(EXE)
	./$(EXE)

$(EXE): $(SRCS)
	ghc Main.hs