
# Process this file with automake to produce Makefile.in
SUBDIRS = bullet

AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src\
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(G3D_CFLAGS)\
	$(YAML_CFLAGS)

AM_LDFLAGS = -version-info 0:0:0 \
	$(G3D_LIBS)\
	$(YAML_LIBS)

lib_LTLIBRARIES = \
	libhkl3d.la

include_HEADERS = hkl3d.h

libhkl3d_la_SOURCES = hkl3d.cpp

libhkl3d_la_LIBADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletmath.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletcollision.la

if CONDITIONAL_BUILD_MULTITHREADED
AM_CXXFLAGS += -DUSE_PARALLEL_DISPATCHER=1
libhkl3d_la_LIBADD += $(top_builddir)/hkl3d/bullet/src/libbulletmultithreaded.la
endif

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
