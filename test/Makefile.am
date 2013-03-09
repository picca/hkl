SUBDIRS = tap hkl
if HAVE_INTROSPECTION
SUBDIRS += bindings
endif
if HKL3D
SUBDIRS += hkl3d
endif

## tap tests

EXTRA_DIST=TESTS

check_PROGRAMS = runtests

AM_CPPFLAGS = \
	-DSOURCE='"$(abs_top_srcdir)"' \
	-DBUILD='"$(abs_top_builddir)"'

check-local: $(check_PROGRAMS)
	./runtests -s $(abs_top_srcdir) $(abs_top_srcdir)/test/TESTS
