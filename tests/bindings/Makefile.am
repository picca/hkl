TESTS_ENVIRONMENT=env GI_TYPELIB_PATH=$(top_builddir)/hkl DATADIR=$(srcdir) $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la
TESTS = python.py trajectory.py polarisation.py

EXTRA_DIST = $(TESTS) crystal.ini

ipython:
	env GI_TYPELIB_PATH=$(top_builddir)/hkl $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la ipython -i -c "from gi.repository import Hkl"

gdb:
	env GI_TYPELIB_PATH=$(top_builddir)/hkl $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la gdb python

CLEANFILES = trajectories.pdf
