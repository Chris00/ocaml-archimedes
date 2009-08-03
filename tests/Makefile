ROOT=..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src
# Dynlink and Bigarray are required (see the META file)
LIBS_CMA = dynlink.cma bigarray.cma archimedes.cma
LIBS_CMXA = dynlink.cmxa bigarray.cmxa archimedes.cmxa

.PHONY: byte opt native
opt: native
byte: backend_text.exe
native: backend_text.com

include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *.exe *.com)
