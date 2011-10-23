## Process this file with automake to produce Makefile.in

AM_LDFLAGS = -no-undefined -version-info 4:0:0
AM_LDFLAGS += $(GSL_LIBS)
AM_CFLAGS = -Wmissing-declarations -std=c89 -ansi -pedantic \
	-I$(top_srcdir) \
	$(GSL_CFLAGS)

hkl_c_sources = \
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

hkl_public_h_sources = \
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

lib_LTLIBRARIES = libhkl.la
libhkl_la_SOURCES = $(hkl_c_sources)
hklincludedir=$(includedir)/hkl
hklinclude_HEADERS = $(hkl_public_h_sources)

# introspection

-include $(INTROSPECTION_MAKEFILE)
INTROSPECTION_GIRS =
INTROSPECTION_SCANNER_ARGS = \
	--add-include-path=$(srcdir) \
	--add-include-path=. \
	--warn-all
INTROSPECTION_COMPILER_ARGS = \
	--includedir=$(srcdir) \
	--includedir=.

if HAVE_INTROSPECTION

libhkl_la_SOURCES += \
	hkl-types.c \
	hkl-type-builtins.c \
	hkl-type-builtins.h

hklinclude_HEADERS += hkl-types.h

introspection_files = \
	hkl-vector.h \
	hkl-matrix.h \
	hkl-quaternion.h \
	hkl-axis.h \
	hkl-source.h \
	hkl-detector.h \
	hkl-detector-factory.h \
	hkl-types.h \
	hkl-vector.c \
	hkl-matrix.c \
	hkl-quaternion.c \
	hkl-axis.c \
	hkl-source.c \
	hkl-detector.c \
	hkl-detector-factory.c \
	hkl-types.c \
	hkl-type-builtins.c \
	hkl-type-builtins.h

AM_LDFLAGS += $(GOBJECT_LIBS)
AM_CFLAGS += $(GOBJECT_CFLAGS)

hkl-type-builtins.h: stamp-hkl-type-builtins.h
	@true
stamp-hkl-type-builtins.h: $(hkl_public_h_sources) $(srcdir)/hkl-type-builtins.h.template
	( cd $(srcdir) && $(GLIB_MKENUMS) --template hkl-type-builtins.h.template \
		$(hkl_public_h_sources) ) >> xgen-gtbh \
	&& (cmp -s xgen-gtbh hkl-type-builtins.h || cp xgen-gtbh hkl-type-builtins.h ) \
	&& rm -f xgen-gtbh \
	&& echo timestamp > $(@F)
hkl-type-builtins.c: $(hkl_public_h_sources) $(srcdir)/hkl-type-builtins.c.template
	( cd $(srcdir) && $(GLIB_MKENUMS) --template hkl-type-builtins.c.template \
		$(hkl_public_h_sources) ) > xgen-gtbc \
	&& cp xgen-gtbc hkl-type-builtins.c  \
	&& rm -f xgen-gtbc

Hkl-4.0.gir: $(INTROSPECTION_SCANNER) libhkl.la Makefile

Hkl_4_0_gir_NAMESPACE = Hkl
Hkl_4_0_gir_VERSION = 4.0
Hkl_4_0_gir_LIBS = libhkl.la
Hkl_4_0_gir_FILES =  $(introspection_files)
Hkl_4_0_gir_PACKAGES = gobject-2.0
Hkl_4_0_gir_CFLAGS = $(INCLUDES) $(AM_CPPFLAGS)
Hkl_4_0_gir_EXPORT_PACKAGES = hkl-4.0
Hkl_4_0_gir_SCANNERFLAGS = --warn-all --c-include='hkl.h'

INTROSPECTION_GIRS += Hkl-4.0.gir

girdir = $(datadir)/gir-1.2
gir_DATA = $(INTROSPECTION_GIRS)

typelibsdir = $(libdir)/girepository-1.0
typelibs_DATA = $(INTROSPECTION_GIRS:.gir=.typelib)

EXTRA_DIST = \
	hkl-type-builtins.h.template \
	hkl-type-builtins.c.template

CLEANFILES = $(gir_DATA) $(typelibs_DATA) stamp-hkl-type-builtins.h hkl-type-builtins.h hkl-type-builtins.c
endif

# Support for GNU Flymake, in Emacs.

check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
