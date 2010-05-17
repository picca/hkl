if HKL3D
SUBDIRS = OpenGL DiffabsDemo ghkl3d
endif

bin_PROGRAMS = ghkl

AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GSL_CFLAGS) \
	$(GTKMM_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(GSL_LIBS) \
	$(GTKMM_LIBS)

if HKL3D
AM_CXXFLAGS += \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/gui/ghkl3d \
	-I$(top_srcdir)/gui/OpenGL \
	-DHKL3D \
	$(GTKGLEXTMM_CFLAGS)

AM_LDFLAGS += \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/gui/ghkl3d/libhkl3d-gui.la \
	$(GTKGLEXTMM_LIBS)
endif

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