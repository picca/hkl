if HKL3D
SUBDIRS = hkl3d
endif

bin_PROGRAMS = ghkl

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(GSL_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GTK_CFLAGS)

AM_LDFLAGS = \
	-Wl,--export-dynamic \
	$(top_builddir)/hkl/libhkl.la \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(GTK_LIBS)

ghkl_LDADD = \
	$(top_builddir)/hkl/libhkl.la

ghkl_SOURCES = \
	hkl-gui-pseudoaxes.c \
	hkl-gui.c

dist_pkgdata_DATA = ghkl.ui pseudo.ui

appldir = ${datadir}/applications
dist_appl_DATA = ghkl.desktop

if HKL3D
AM_CFLAGS += \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/gui/hkl3d \
	-I$(top_srcdir)/gui/OpenGL \
	-DHKL3D \
	$(G3D_CFLAGS) \
	$(GTKGLEXT_CFLAGS)

AM_LDFLAGS += \
	$(G3D_LIBS) \
	$(GTKGLEXT_LIBS)

ghkl_LDADD += \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletdynamics.la \
	$(top_builddir)/gui/hkl3d/libhkl3d-gui.la


ghkl_SOURCES += hkl-gui-3d.c

dist_pkgdata_DATA += 3d.ui

endif

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
