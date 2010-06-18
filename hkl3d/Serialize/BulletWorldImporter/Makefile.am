# Process this file with automake to produce Makefile.in

AM_CXXFLAGS = \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/hkl3d/Serialize/BulletFileLoader

noinst_LTLIBRARIES = libbulletWorldImporter.la

libbulletWorldImporter_la_SOURCES = \
	btBulletWorldImporter.cpp \
	btBulletWorldImporter.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
