AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	$(G3D_CFLAGS) \
	$(GTKGLEXTMM_CFLAGS) \
	-DG_DISABLE_DEPRECATED \
	-DGDK_DISABLE_DEPRECATED \
	-DGDK_PIXBUF_DISABLE_DEPRECATED	\
	-DGTK_DISABLE_DEPRECATED

AM_CFLAGS = $(AM_CXXFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl3d/libhkl3d.la \
	$(top_builddir)/hkl3d/bullet/src/libbulletdynamics.la \
	$(G3D_LIBS) \
	$(GLU_LIBS) \
	$(GTKGLEXTMM_LIBS)

noinst_LTLIBRARIES = libhkl3d-gui.la
libhkl3d_gui_la_SOURCES = \
	trackball.h \
	trackball.c
#	texture.h \
#	texture.c \
#	hkl3d-gui-gl.h \
#	hkl3d-gui-gl.c \
#	hkl3d-gui-model.h \
#	hkl3d-gui-model.cpp \
#	trackball.h \
#	trackball.c \
#	GL_ShapeDrawer.h \
#	GL_ShapeDrawer.cpp

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
