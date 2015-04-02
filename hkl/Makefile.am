## Process this file with automake to produce Makefile.in
SUBDIRS = ccan

AM_LDFLAGS = \
	-no-undefined -version-info 5:0:0 \
	$(GLIB_LIBS) \
	$(GSL_LIBS)
AM_CFLAGS = \
	-DG_LOG_DOMAIN=\"Hkl\" \
	-I$(top_srcdir) \
	-I$(top_builddir) \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

hkl_c_sources = \
	hkl-axis.c \
	hkl-detector.c \
	hkl-detector-factory.c \
	hkl-factory.c \
	hkl-geometry.c \
	hkl-interval.c \
	hkl-lattice.c \
	hkl-macros.c \
	hkl-matrix.c \
	hkl-parameter.c \
	hkl-pseudoaxis.c \
	hkl-pseudoaxis-auto.c \
	hkl-pseudoaxis-common-eulerians.c \
	hkl-pseudoaxis-common-hkl.c \
	hkl-pseudoaxis-common-psi.c \
	hkl-pseudoaxis-common-q.c \
	hkl-pseudoaxis-common-tth.c \
	hkl-engine-2c.c \
	hkl-engine-e4c.c \
	hkl-engine-k4c.c \
	hkl-engine-e6c.c \
	hkl-engine-k6c.c \
	hkl-engine-petra3-p09-eh2.c \
	hkl-engine-soleil-sirius-turret.c \
	hkl-engine-soleil-sixs-med.c \
	hkl-engine-zaxis.c \
	hkl-quaternion.c \
	hkl-sample.c \
	hkl-source.c \
	hkl-unit.c \
	hkl-vector.c

hkl_public_h_sources = ../hkl.h

hkl_private_h_sources = \
	hkl-axis-private.h \
	hkl-detector-private.h \
	hkl-factory-private.h \
	hkl-geometry-private.h \
	hkl-interval-private.h \
	hkl-lattice-private.h \
	hkl-macros-private.h \
	hkl-matrix-private.h \
	hkl-parameter-private.h \
	hkl-pseudoaxis-private.h \
	hkl-pseudoaxis-auto-private.h \
	hkl-pseudoaxis-common-eulerians-private.h \
	hkl-pseudoaxis-common-hkl-private.h \
	hkl-pseudoaxis-common-psi-private.h \
	hkl-pseudoaxis-common-q-private.h \
	hkl-pseudoaxis-common-tth-private.h \
	hkl-quaternion-private.h \
	hkl-sample-private.h \
	hkl-source-private.h \
	hkl-unit-private.h \
	hkl-vector-private.h

lib_LTLIBRARIES = libhkl.la
libhkl_la_SOURCES = $(hkl_c_sources) $(hkl_private_h_sources)
libhkl_la_LIBADD = ccan/libccan.la
libhkl_la_CFLAGS = \
	$(AM_CFLAGS) \
	-Wno-initializer-overrides \
	-Wno-unused-result # \ do not activate visibility yet.
	-fvisibility=hidden

# just compile the template diffractometer to be sure that it is valid
noinst_LTLIBRARIES = libdummy.la
libdummy_la_SOURCES = hkl-engine-template.c

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
	hkl-types.h \
	hkl-type-builtins.c \
	hkl-type-builtins.h \
	hkl-binding.c \
	hkl-binding-private.h

introspection_files = \
	../hkl.h \
	hkl-binding-private.h \
	hkl-types.h \
	hkl-matrix.c \
	hkl-parameter.c \
	hkl-axis.c \
	hkl-geometry.c \
	hkl-detector.c \
	hkl-detector-factory.c \
	hkl-lattice.c \
	hkl-sample.c \
	hkl-pseudoaxis.c \
	hkl-factory.c \
	hkl-binding.c \
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
hkl-type-builtins.c: $(hkl_public_h_sources) $(srcdir)/hkl-type-builtins.c.template hkl-type-builtins.h
	( cd $(srcdir) && $(GLIB_MKENUMS) --template hkl-type-builtins.c.template \
		$(hkl_public_h_sources) ) > xgen-gtbc \
	&& cp xgen-gtbc hkl-type-builtins.c  \
	&& rm -f xgen-gtbc

Hkl-@VMAJ@.0.gir: $(INTROSPECTION_SCANNER) libhkl.la Makefile

Hkl_@VMAJ@_0_gir_NAMESPACE = Hkl
Hkl_@VMAJ@_0_gir_VERSION = @VMAJ@.0
Hkl_@VMAJ@_0_gir_LIBS = libhkl.la
Hkl_@VMAJ@_0_gir_FILES =  $(introspection_files)
Hkl_@VMAJ@_0_gir_PACKAGES = gobject-2.0
Hkl_@VMAJ@_0_gir_CFLAGS = $(INCLUDES) $(AM_CPPFLAGS) $(AM_CFLAGS)
Hkl_@VMAJ@_0_gir_EXPORT_PACKAGES = hkl-@VMAJ@.0
Hkl_@VMAJ@_0_gir_SCANNERFLAGS = --warn-all --c-include='hkl.h'

INTROSPECTION_GIRS += Hkl-@VMAJ@.0.gir

girdir = $(datadir)/gir-1.0
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

# check for includes with iwyu
iwyu:
	@list='$(libhkl_la_SOURCES)';\
	for p in $$list; do\
		iwyu $(AM_CFLAGS) $$p; \
	done;

.PHONY: check-syntax iwyu
