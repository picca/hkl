check_PROGRAMS = test-t

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/test \
	$(G3D_CFLAGS)

# The bits below are for the tap hkl3d test suite.
test_t_LDADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/test/tap/libtap.a

test_t_SOURCES = main.c

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax