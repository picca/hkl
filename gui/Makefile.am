bin_PROGRAMS = ghkl

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GTK_CFLAGS) \
	$(GEE_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(GTK_LIBS) \
	$(GEE_LIBS)

AM_VALAFLAGS = --pkg gtk+-2.0 --pkg gee-1.0

ghkl_SOURCES = \
	pseudoaxesframe.vala \
	ghkl.vala \
	hkl.vapi \
	main.c

dist_pkgdata_DATA = ghkl.ui pseudo.ui

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax