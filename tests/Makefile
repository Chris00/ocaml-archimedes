ROOT=..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src
# Dynlink and Bigarray are required (see the META file)
LIBS_CMA = dynlink.cma bigarray.cma archimedes.cma
LIBS_CMXA = dynlink.cmxa bigarray.cmxa archimedes.cmxa

.PHONY: all byte opt native

include Makefile.in

all: byte native
opt: native
byte: $(addsuffix .exe,$(TESTS))
native:  $(addsuffix .com,$(TESTS))

include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *.exe *.com)
