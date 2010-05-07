# Process this file with automake to produce Makefile.in

AM_CXXFLAGS = -I$(top_srcdir)/hkl3d/bullet/src

noinst_LTLIBRARIES = libconvexdecomposition.la

libconvexdecomposition_la_SOURCES = \
	concavity.cpp \
	ConvexDecomposition.cpp \
	vlookup.cpp \
	bestfit.cpp \
	ConvexBuilder.cpp \
	cd_hull.cpp \
	raytri.cpp \
	splitplane.cpp \
	float_math.cpp \
	planetri.cpp \
	cd_wavefront.cpp \
	bestfitobb.cpp \
	meshvolume.cpp \
	fitsphere.cpp \
	fitsphere.h \
	vlookup.h \
	concavity.h \
	ConvexDecomposition.h \
	bestfit.h \
	cd_vector.h \
	ConvexBuilder.h \
	cd_hull.h \
	raytri.h \
	splitplane.h \
	float_math.h \
	planetri.h \
	cd_wavefront.h \
	bestfitobb.h \
	meshvolume.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
