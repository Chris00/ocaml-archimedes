AC_INIT(cairo, 0.2, christophe.troestler@umons.ac.be)

AC_MSG_RESULT([configuring $PACKAGE_STRING])

# OCaml
########################################################################

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_OS_TYPE

AC_CHECK_OCAML_PKG([graphics])
if test "OCAML_PKG_graphics" != "no"; then
   GRAPHICS_PATH=`$OCAMLFIND query graphics`
   AC_SUBST([GRAPHICS_PATH])
fi

AC_CHECK_OCAML_PKG([cairo2])
if test "OCAML_PKG_cairo2" != "no"; then
   CAIRO2_PATH=`$OCAMLFIND query cairo2`
   AC_SUBST([CAIRO2_PATH])
fi

# substitutions to perform
AC_SUBST(PACKAGE_NAME)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)

# Finally create the Makefile and samples (read only to avoid changing
# them by mistake)
AC_CONFIG_FILES([Makefile.ocaml],[chmod a-w Makefile.ocaml])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([Makefile], [chmod a-w Makefile])
AC_CONFIG_FILES([src/Makefile], [chmod a-w src/Makefile])
AC_CONFIG_FILES([src/archimedes_graphics.dep])
AC_CONFIG_FILES([src/archimedes_cairo.dep])
AC_OUTPUT
