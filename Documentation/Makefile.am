SUBDIRS=api figures sphinx

dist_man_MANS = ghkl.1

AM_LDFLAGS = $(top_builddir)/hkl/libhkl.la

dist_html_DATA=hkl.html

.html.org:
	env GI_TYPELIB_PATH=$(top_builddir)/hkl\
	$(LIBTOOL) --mode=execute -dlopen $(AM_LDFLAGS)\
	$(EMACS) $< --batch -q --load $(srcdir)/default.el -f org-html-export-to-html --kill

EXTRA_DIST=default.el
