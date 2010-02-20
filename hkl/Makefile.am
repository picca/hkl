## Process this file with automake to produce Makefile.in

AM_LDFLAGS = -no-undefined -version-info 4:0:0
AM_LDFLAGS += $(GSL_LIBS)
AM_CFLAGS = -I$(top_srcdir) $(GSL_CFLAGS)

lib_LTLIBRARIES = \
	libhkl.la

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

include_HEADERS = \
	hkl.h

hklincludedir=$(includedir)/hkl
hklinclude_HEADERS = \
	hkl-axis.h \
	hkl-detector.h \
	hkl-detector-factory.h \
	hkl-error.h \
	hkl-geometry.h \
	hkl-geometry-factory.h \
	hkl-interval.h \
	hkl-parameter.h \
	hkl-lattice.h \
	hkl-list.h \
	hkl-macros.h \
	hkl-matrix.h \
	hkl-pseudoaxis.h \
	hkl-pseudoaxis-auto.h \
	hkl-pseudoaxis-common.h \
	hkl-pseudoaxis-common-eulerians.h \
	hkl-pseudoaxis-common-hkl.h \
	hkl-pseudoaxis-common-psi.h \
	hkl-pseudoaxis-common-q.h \
	hkl-pseudoaxis-e4cv.h \
	hkl-pseudoaxis-e6c.h \
	hkl-pseudoaxis-factory.h  \
	hkl-pseudoaxis-k4cv.h \
	hkl-pseudoaxis-k6c.h \
	hkl-pseudoaxis-zaxis.h \
	hkl-quaternion.h \
	hkl-sample.h \
	hkl-source.h \
	hkl-unit.h \
	hkl-vector.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
