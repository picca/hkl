SUBDIRS=api figures sphinx

dist_man_MANS = ghkl.1

# We require automake 1.6 at least.
AUTOMAKE_OPTIONS = 1.6

DOC_ENVIRONMENT=env GI_TYPELIB_PATH=$(top_builddir)/hkl

AM_LDFLAGS = $(top_builddir)/hkl/libhkl.la

DOC_MODULE=hkl

DOC_EMACS_DEFAULT_EL = default.el

include $(top_srcdir)/org-doc.make
