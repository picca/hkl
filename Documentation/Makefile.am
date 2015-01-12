SUBDIRS=api figures sphinx

dist_man_MANS = ghkl.1

hkl.html: hkl.org
	-@EMACS@ $< --batch -f org-html-export-to-html --kill

dist_doc_DATA = hkl.html
