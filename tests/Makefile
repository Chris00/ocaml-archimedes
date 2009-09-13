ROOT=..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src
# Dynlink and Bigarray are required (see the META file)
LIBS_CMA = dynlink.cma bigarray.cma archimedes.cma
LIBS_CMXA = dynlink.cmxa bigarray.cmxa archimedes.cmxa

TESTS = backend_text.ml text.ml marks.ml viewports.ml test_function.ml

.PHONY: all byte opt native
all: byte native
opt: native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)

include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *.exe *.com)
