TESTS_ENVIRONMENT=env GI_TYPELIB_PATH=$(top_builddir)/hkl $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la
TESTS = python.py trajectory.py

EXTRA_DIST = $(TESTS)

ipython:
	env GI_TYPELIB_PATH=$(top_builddir)/hkl $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la ipython

CLEANFILES = trajectories.pdf