# Compiler :
CC=ocamlopt
LDFLAGS=
CFLAGS=
MAIN=main.ml
SOURCES=
PACKAGES=unix camlimages.png

OBJECTS=$(SOURCES)
OUTPUT=$(MAIN:.ml=.bin)

all: $(OBJECTS)
	ocamlfind $(CC) -package `echo "$(PACKAGES)" | sed 's/ / -package /g'` -linkpkg $(SOURCES) $(MAIN) -o $(OUTPUT) 

clean:
	rm *.cmi *.cmx *.o $(OUTPUT)
