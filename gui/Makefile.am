if HKL3D
SUBDIRS = hkl3d
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
	-I$(top_srcdir)/gui/hkl3d \
	-I$(top_srcdir)/gui/OpenGL \
	-DHKL3D \
	$(GTKGLEXTMM_CFLAGS)

AM_LDFLAGS += \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/gui/hkl3d/libhkl3d-gui.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletdynamics.la \
	$(GTKGLEXTMM_LIBS)
endif

ghkl_SOURCES = \
	hkl-gui.h \
	hkl-gui.cpp \
	hkl-gui-callbacks.cpp \
	hkl-gui-pseudoaxes.h \
	hkl-gui-pseudoaxes.cpp \
	hkl-gui-modelcolumns.h \
	main.cpp

dist_pkgdata_DATA = ghkl.ui pseudo.ui

appldir = ${datadir}/applications
dist_appl_DATA = ghkl.desktop

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax