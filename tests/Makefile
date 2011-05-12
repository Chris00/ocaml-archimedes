ROOT=..
include $(ROOT)/Makefile.conf

# Dynlink and Bigarray are required (see the META file)
OCAMLPACKS = dynlink,bigarray
OCAMLINCLUDES = $(ROOT)/src

LIBS_CMA = archimedes.cma
LIBS_CMXA = archimedes.cmxa

#TESTS = backend_line.ml viewport_path.ml marks.ml test_layout.ml \
#  test_custom_layout.ml viewport_autofit.ml test_axes.ml test_box.ml \
#  test_cross.ml test_plot.ml layout_sync.ml test_arrows.ml vector_field.ml \
#	test_xy_param.ml test_boxes.ml test_two_viewports.ml test_stack.ml
TESTS = test_stack.ml test_plot.ml

#viewports.ml 
.PHONY: all byte opt native
all: byte native
opt: native
byte: $(TESTS:.ml=.exe)
native: $(TESTS:.ml=.com)


include $(ROOT)/Makefile.ocaml

clean::
	$(RM) $(wildcard *.exe *.com)
