ROOT=..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src /usr/lib/ocaml/oUnit
# Dynlink and Bigarray are required (see the META file)
LIBS_CMA = unix.cma oUnit.cma dynlink.cma bigarray.cma archimedes.cma
LIBS_CMXA = unix.cmxa oUnit.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa

UNITTESTS = coordinate.ml

all: byte native
opt: native
byte: $(UNITTESTS:.ml=.exe)
native: $(UNITTESTS:.ml=.com)

include $(ROOT)/Makefile.ocaml
