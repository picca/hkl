## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = \
	libhkl.la

libhkl_la_LDFLAGS = -version-info 4:0:0
libhkl_la_LIBS = $(GSL_LIBS)
libhkl_la_CFLAGS = \
	-I$(top_srcdir)/include \
	-I$(top_srcdir)/include/hkl \
	$(GSL_CFLAGS)

libhkl_la_SOURCES = \
	hkl-error.c \
	hkl-axis.c \
	hkl-detector.c \
	hkl-detector-factory.c \
	hkl-geometry.c \
	hkl-geometry-factory.c \
	hkl-interval.c \
	hkl-lattice.c \
	hkl-matrix.c \
	hkl-parameter.c \
	hkl-pseudoaxis.c \
	hkl-pseudoaxis-auto.c \
	hkl-pseudoaxis-common.c \
	hkl-pseudoaxis-common-eulerians.c \
	hkl-pseudoaxis-common-hkl.c \
	hkl-pseudoaxis-common-psi.c \
	hkl-pseudoaxis-common-q.c \
	hkl-pseudoaxis-e4cv-hkl.c \
	hkl-pseudoaxis-e4cv-psi.c \
	hkl-pseudoaxis-e6c-hkl.c \
	hkl-pseudoaxis-e6c-psi.c \
	hkl-pseudoaxis-factory.c \
	hkl-pseudoaxis-k4cv-hkl.c \
	hkl-pseudoaxis-k4cv-psi.c \
	hkl-pseudoaxis-k6c-hkl.c \
	hkl-pseudoaxis-k6c-psi.c \
	hkl-pseudoaxis-zaxis-hkl.c \
	hkl-quaternion.c \
	hkl-sample.c \
	hkl-source.c \
	hkl-unit.c \
	hkl-vector.c \
	usage.c

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe $(libhkl_la_CFLAGS)
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
