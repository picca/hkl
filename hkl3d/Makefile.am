# Process this file with automake to produce Makefile.in

AM_CXXFLAGS = \
	-fpermissive \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src\
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(G3D_CFLAGS) \
	$(YAML_CFLAGS) \
	$(BULLET_CFLAGS)

AM_LDFLAGS = -version-info 0:0:0 \
	$(G3D_LIBS) \
	$(YAML_LIBS) \
	$(BULLET_LIBS)

lib_LTLIBRARIES = libhkl3d.la
libhkl3d_la_SOURCES = hkl3d.cpp
libhkl3d_la_LIBADD = \
	$(top_builddir)/hkl/libhkl.la

noinst_HEADERS = hkl3d.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
