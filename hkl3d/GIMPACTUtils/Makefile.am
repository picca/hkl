# Process this file with automake to produce Makefile.in

AM_CXXFLAGS = \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/hkl3d/ConvexDecomposition

noinst_LTLIBRARIES = libgimpactutils.la

libgimpactutils_la_SOURCES = \
	btGImpactConvexDecompositionShape.cpp \
	btGImpactConvexDecompositionShape.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
