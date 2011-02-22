ROOT=..
include $(ROOT)/Makefile.conf

# Dynlink and Bigarray are required (see the META file)
OCAMLPACKS = dynlink,bigarray
OCAMLINCLUDES = $(ROOT)/src

TESTS = backend_text.ml backend_path.ml backend_line.ml \
  text.ml marks.ml functions.ml simple_plot.ml
#viewports.ml 
.PHONY: all byte opt native
all: byte native
opt: native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)

backend_path.exe: LIBS_CMA = string_utils.cmo conf.cmo color.cmo backend.cmo
backend_path.com: LIBS_CMXA = string_utils.cmx conf.cmx color.cmx backend.cmx



include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *.exe *.com)
