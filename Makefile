WEB = shell.forge.ocamlcore.org:/home/groups/archimedes/htdocs

PKGNAME = $(shell oasis query name)
PKGVERSION = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

COMA = ,
FILESAB = $(subst $(COMA),,$(shell oasis query FilesAB))

DISTFILES = AUTHORS.txt INSTALL.txt README.txt _oasis _tags myocamlbuild.ml \
  setup.ml Makefile src/META src/API.odocl \
  archimedes.install _oasis_remove_.ml \
  $(wildcard $(addprefix src/, *.ml *.mli *.mllib *.mlpack *.ab)) \
  $(wildcard tests/*.ml tests/*.ab)  $(wildcard examples/*.ml)

.PHONY: all byte native configure doc test \
  install uninstall reinstall upload-doc

all byte native: setup.data
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml $(FILESAB)
	ocaml $< -configure --enable-tests --enable-cairo2

setup.ml AUTHORS.txt INSTALL.txt README.txt: _oasis
	oasis setup -setup-update dynamic

doc install uninstall reinstall test: all
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -r _build/src/API.docdir/ $(WEB)/


# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
# Generate a compilation files not depending on oasis:
	cd $(PKGNAME)-$(PKGVERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

.PHONY: clean distclean dist-clean
clean:
	-ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg) setup.data

distclean dist-clean:: clean
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
	$(RM) $(subst .ab,,$(FILESAB))
