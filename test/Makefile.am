
TESTS = $(check_PROGRAMS)

check_PROGRAMS = test bench

AM_CPPFLAGS = -std=c99 -pedantic -D_BSD_SOURCE\
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl

AM_LDFLAGS = $(top_builddir)/hkl/libhkl.la $(GSL_LIBS) -lm

test_SOURCES = \
hkl-test.h \
hkl-test-error.c \
hkl-test-pseudoaxis.c \
hkl-test-quaternion.c \
hkl-test-axis.c \
hkl-test-interval.c \
hkl-test-pseudoaxis-e4cv.c \
hkl-test-sample.c \
hkl-test.c \
hkl-test-lattice.c \
hkl-test-pseudoaxis-e6c.c \
hkl-test-source.c \
hkl-test-detector.c \
hkl-test-matrix.c \
hkl-test-pseudoaxis-k4cv.c \
hkl-test-vector.c \
hkl-test-geometry.c \
hkl-test-parameter.c \
hkl-test-pseudoaxis-k6c.c \
main.c

bench_SOURCES = \
	bench.c

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
