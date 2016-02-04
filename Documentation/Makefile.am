SUBDIRS=api figures sphinx

dist_man_MANS = ghkl.1

AM_LDFLAGS = $(top_builddir)/hkl/libhkl.la

dist_html_DATA=hkl.html

hkl.html: hkl.org $(srcdir)/hkl-default.el
	env GI_TYPELIB_PATH=$(top_builddir)/hkl \
	$(LIBTOOL) --mode=execute -dlopen $(AM_LDFLAGS) \
	$(EMACS) $< --batch -q --load $(srcdir)/hkl-default.el -f org-html-export-to-html --debug-init --kill

EXTRA_DIST=hkl-default.el

CLEANFILES=hkl.html

doc-edit:
	env GI_TYPELIB_PATH=$(top_builddir)/hkl \
	$(LIBTOOL) --mode=execute -dlopen $(AM_LDFLAGS) \
	$(EMACS) hkl.org.in -q --load $(srcdir)/hkl-default.el

doc-show: hkl.html
	sensible-browser $(builddir)/hkl.html

.PHONY: doc-edit doc-show
