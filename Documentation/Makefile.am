SUBDIRS=api figures sphinx

dist_man_MANS = ghkl.1

install-doc: hkl.html

hkl.html: hkl.org
	emacs $< --batch -f org-html-export-to-html --kill
