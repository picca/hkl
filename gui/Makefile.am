bin_PROGRAMS = ghkl

AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GTKMM_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(GTKMM_LIBS)

ghkl_SOURCES = \
	axespinbutton.h \
	axespinbutton.cpp \
	hklwindow.h \
	hklwindow.cpp \
	pseudoaxespinbutton.h \
	pseudoaxespinbutton.cpp \
	modelcolumns.h \
	main.cpp

dist_pkgdata_DATA = hkl.ui

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax