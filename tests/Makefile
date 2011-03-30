ROOT=..
include $(ROOT)/Makefile.conf

# Dynlink and Bigarray are required (see the META file)
OCAMLPACKS = dynlink,bigarray
OCAMLINCLUDES = $(ROOT)/src

LIBS_CMA = archimedes.cma
LIBS_CMXA = archimedes.cmxa

TESTS = arrays.ml backend_text.ml backend_path.ml backend_line.ml \
  text.ml marks.ml functions.ml simple_plot.ml
#viewports.ml 
.PHONY: all byte opt native
all: byte native
opt: native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)


include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *.exe *.com)
