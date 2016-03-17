OCAMLC=ocamlbuild
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLFLAGS=-I +sdl -I +site-lib/sdl
OCAMLLD=bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlgfx.cmxa sdlttf.cmxa
OUTPUT=YolOCR

# List of files
OCR_OBJS= Sdl_tools.ml pixel.ml matrix.ml block.ml tree.ml levenshtein.ml filters.ml crop.ml otsu.ml binarys.ml resize.ml adjust.ml rotate.ml rlsa.ml segmentation.ml skynet.ml main.ml

all: $(OCR_OBJS)
			$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLLD) -o $(OUTPUT) $(OCR_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
			$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
			$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
			$(OCAMLOPT) -c $<

# Run
run: all
			./YolOCR

# Clean up
clean:
			rm -f YolOCR
			rm -f *.cm[iox]
			rm -f *.o
			rm -f output.bmp
		    rm -f *[0-9]	
