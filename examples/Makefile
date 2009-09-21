ROOT=..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src
# Dynlink and Bigarray are required (see the META file)
LIBS_CMA = dynlink.cma bigarray.cma archimedes.cma
LIBS_CMXA = dynlink.cmxa bigarray.cmxa archimedes.cmxa

EXAMPLES = simple.ml

all: byte native
byte: $(EXAMPLES:.ml=.exe)
native: $(EXAMPLES:.ml=.com)

include $(ROOT)/Makefile.ocaml
