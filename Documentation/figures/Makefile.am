.pov.png:
	@POVRAY@ +P +H600 +W800 -D +Q9 $<

.asy.pdf:
	@ASY@ -config "" -render=0 -f pdf -noprc -o $@ $<

dist_pkgdata_DATA = \
	3S+1D.png \
	B_b3_y.png \
	B_a.pdf \
	B_b.pdf \
	cristal.png \
	4S+2D.png \
	4S+2D_reciproque.png \
	4S+2D_reciproque2.png \
	4S+2D_reciproque3.png \
	4S+2D_reciproque4.png \
	test.png \
	zaxis.pdf

CLEANFILES = $(dist_pkgdata_DATA)

EXTRA_DIST = \
	3S+1D.pov \
	B_b3_y.pov \
	B_a.asy \
	B_b.asy \
	cristal.pov \
	4S+2D.pov \
	4S+2D_reciproque.pov \
	4S+2D_reciproque2.pov \
	4S+2D_reciproque3.pov \
	4S+2D_reciproque4.pov \
	test.pov \
	arrows.inc \
	bravais.inc \
	cristal.ini \
	zaxis.asy
