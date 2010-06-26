bin_PROGRAMS = ghkl

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GTK_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(GTK_LIBS)

ghkl_SOURCES = \
	ghkl.h \
	ghkl.c \
	ghkl-callbacks.c \
	pseudoaxesframe.h \
	pseudoaxesframe.c \
	modelcolumns.h \
	main.c

dist_pkgdata_DATA = ghkl.ui pseudo.ui

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax