TESTS = $(check_PROGRAMS)

check_PROGRAMS = test

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletdynamics.la

AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	$(G3D_CFLAGS)

test_SOURCES = main.cpp

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax