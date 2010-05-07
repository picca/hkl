noinst_LTLIBRARIES = libbulletopenglsupport.la

AM_CXXFLAGS= \
	-I$(top_srcdir)/hkl3d/bullet/src

libbulletopenglsupport_la_SOURCES = \
	DemoApplication.cpp \
	GLDebugDrawer.h \
	GL_Simplex1to4.cpp \
	GLDebugFont.cpp \
	GLDebugFont.h \
	GlutDemoApplication.cpp \
	GlutDemoApplication.h \
	GlutStuff.h \
	DemoApplication.h \
	GL_ShapeDrawer.cpp \
	GL_Simplex1to4.h \
	RenderTexture.cpp \
	DebugCastResult.h \
	GLDebugDrawer.cpp \
	GL_ShapeDrawer.h \
	GlutStuff.cpp \
	RenderTexture.h

libbulletopenglsupport_la_LIBADD = \
	$(top_builddir)/hkl3d/bullet/src/libbulletdynamics.la

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
