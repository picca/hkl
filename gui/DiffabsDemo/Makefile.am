AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-I$(top_srcdir)/gui/OpenGL \
	$(G3D_CFLAGS) \
	$(GLU_CFLAGS) 

AM_LDFLAGS = \
	$(G3D_LIBS) \
	$(GLU_LIBS) \
	$(GTKGLEXTMM_LIBS) \
	-lglut

noinst_PROGRAMS = DiffabsDemo

DiffabsDemo_SOURCES = \
	DiffabsDemo.cpp \
	DiffabsDemo.h \
	main.cpp

DiffabsDemo_LDADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/gui/OpenGL/libbulletopenglsupport.la

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
