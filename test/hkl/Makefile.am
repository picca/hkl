check_PROGRAMS = \
	unit-t \
	bench-t \
	axis-t \
	error-t \
	pseudoaxis-t \
	quaternion-t \
	interval-t \
	pseudoaxis-e4cv-t \
	pseudoaxis-e4ch-t \
	sample-t \
	lattice-t \
	pseudoaxis-e6c-t \
	source-t \
	detector-t \
	matrix-t \
	pseudoaxis-k4cv-t \
	vector-t \
	geometry-t \
	parameter-t \
	pseudoaxis-k6c-t \
	pseudoaxis-zaxis-t \
	pseudoaxis-soleil-sixs-med-t

AM_CPPFLAGS = -Wextra -pedantic -D_BSD_SOURCE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/test

LDADD = $(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/test/tap/libtap.a \
	$(GSL_LIBS)

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
