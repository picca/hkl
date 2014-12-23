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
	hkl-gui-macros.h \
	hkl-gui-pseudoaxes.c \
	hkl-gui-pseudoaxes.h \
	hkl-gui.c \
	hkl-gui.h

dist_pkgdata_DATA = ghkl.ui pseudo.ui

appldir = ${datadir}/applications
dist_appl_DATA = ghkl.desktop

if HKL3D
AM_CFLAGS += \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-DHKL3D \
	$(G3D_CFLAGS) \
	$(GTKGLEXT_CFLAGS) \
	$(BULLET_CFLAGS)

AM_LDFLAGS += -lstdc++\
	$(G3D_LIBS) \
	$(GTKGLEXT_LIBS) \
	$(BULLET_LIBS)

ghkl_LDADD += \
	$(top_builddir)/hkl3d/libhkl3d.la

ghkl_SOURCES += \
	hkl-gui-3d-gl.c \
	hkl-gui-3d-gl.h \
	hkl-gui-3d.c \
	hkl-gui-3d.h

dist_pkgdata_DATA += 3d.ui

endif

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
