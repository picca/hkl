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
	$(G3D_LIBS) \
	$(GLU_LIBS) \
	$(GTKGLEXTMM_LIBS) \
	-lglut

lib_LTLIBRARIES = libhkl3d-gui.la
libhkl3d_gui_la_SOURCES = \
	texture.h \
	texture.c \
	hkl3d-gui-gl.h \
	hkl3d-gui-gl.c \
	hkl3d-gui-view.h \
	hkl3d-gui-view.cpp \
	hkl3d-gui-model.h \
	hkl3d-gui-model.cpp \
	hkl3d-gui-scene.h \
	hkl3d-gui-scene.cpp \
	trackball.h \
	trackball.c \
	GL_ShapeDrawer.h \
	GL_ShapeDrawer.cpp

# build the stand alone ghkl3d application
bin_PROGRAMS = ghkl3d

ghkl3d_SOURCES = \
	main.cpp \
	hkl3d-gui-application.h \
	hkl3d-gui-application.cpp

ghkl3d_LDADD = libhkl3d-gui.la

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
