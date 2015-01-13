SUBDIRS=api figures sphinx

dist_man_MANS = ghkl.1

hkl.html: hkl.org
	-env GI_TYPELIB_PATH=$(top_builddir)/hkl \
	$(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la \
	@EMACS@ $< --batch -q --load default.el -f org-html-export-to-html --kill

dist_doc_DATA = hkl.html
