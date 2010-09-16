if HKL3D
SUBDIRS = hkl3d
endif

bin_PROGRAMS = ghkl

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GSL_CFLAGS) \
	$(GTK_CFLAGS) \
	$(GEE_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(GSL_LIBS) \
	$(GTK_LIBS) \
	$(GEE_LIBS)

AM_VALAFLAGS = --pkg gtk+-2.0 --pkg gee-1.0

ghkl_LDADD = \
	$(top_builddir)/hkl/libhkl.la

ghkl_SOURCES = \
	hkl-gui-pseudoaxes.vala \
	hkl-gui.vala \
	hkl.vapi

if HKL3D
AM_CXXFLAGS += \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/gui/hkl3d \
	-I$(top_srcdir)/gui/OpenGL \
	-DHKL3D \
	$(GTKGLEXTMM_CFLAGS)

AM_LDFLAGS += \
	$(GTKGLEXTMM_LIBS)

ghkl_LDADD += \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/gui/hkl3d/libhkl3d-gui.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletdynamics.la

ghkl_SOURCES += \
	hkl-gui-3d.h \
	hkl-gui-3d.cpp

endif

dist_pkgdata_DATA = ghkl.ui pseudo.ui

appldir = ${datadir}/applications
dist_appl_DATA = ghkl.desktop

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax