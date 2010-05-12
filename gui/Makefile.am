SUBDIRS = OpenGL DiffabsDemo ghkl3d

bin_PROGRAMS = ghkl

AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/gui/ghkl3d \
	-I$(top_srcdir)/gui/OpenGL \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GTKMM_CFLAGS) \
	$(GTKGLEXTMM_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/gui/ghkl3d/libhkl3d-gui.la \
	$(GTKMM_LIBS) \
	$(GTKGLEXTMM_LIBS)

ghkl_SOURCES = \
	ghkl.h \
	ghkl.cpp \
	ghkl-callbacks.cpp \
	pseudoaxesframe.h \
	pseudoaxesframe.cpp \
	modelcolumns.h \
	main.cpp

dist_pkgdata_DATA = ghkl.ui pseudo.ui

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax