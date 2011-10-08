## Process this file with automake to produce Makefile.in

AM_LDFLAGS = -no-undefined -version-info 4:0:0
AM_LDFLAGS += $(GSL_LIBS)
AM_CFLAGS = -Wmissing-declarations -std=c89 -ansi -pedantic \
	-I$(top_srcdir) \
	$(GSL_CFLAGS)

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
	hkl-macros.c \
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
	hkl-pseudoaxis-soleil-sixs-med.c \
	hkl-pseudoaxis-petra3-hkl.c \
	hkl-quaternion.c \
	hkl-sample.c \
	hkl-source.c \
	hkl-unit.c \
	hkl-vector.c

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
	hkl-pseudoaxis-soleil-sixs-med.h \
	hkl-pseudoaxis-petra3.h \
	hkl-quaternion.h \
	hkl-sample.h \
	hkl-source.h \
	hkl-unit.h \
	hkl-vector.h

# introspection

-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = \
	--add-include-path=../hkl \
	--warn-all
INTROSPECTION_COMPILER_ARGS = \
	--includedir=$(srcdir) \
	--includedir=. \
	--includedir=../hkl

if HAVE_INTROSPECTION

Hkl-4.0.gir: $(INTROSPECTION_SCANNER) libhkl.la Makefile

Hkl_4_0_gir_NAMESPACE = Hkl
Hkl_4_0_gir_VERSION = 4.0
Hkl_4_0_gir_LIBS = libhkl.la
Hkl_4_0_gir_FILES = hkl-vector.h hkl-vector.c
Hkl_4_0_gir_PACKAGES = gsl
Hkl_4_0_gir_CFLAGS = $(INCLUDES) $(AM_CPPFLAGS)
Hkl_4_0_gir_EXPORT_PACKAGES = hkl-4.0
Hkl_4_0_gir_SCANNERFLAGS = --warn-all --c-include='hkl.h'

INTROSPECTION_GIRS += Hkl-4.0.gir

girdir = $(datadir)/gir-1.0
gir_DATA = $(INTROSPECTION_GIRS)

typelibsdir = $(libdir)/girepository-1.0
typelibs_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

CLEANFILES = $(gir_DATA) $(typelibs_DATA)
endif

# Support for GNU Flymake, in Emacs.

check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
