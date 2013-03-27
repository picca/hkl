.asy.png:
	@ASY@ -config "" -render=4 -f png -o $@ $<

.asy.pdf:
	@ASY@ -config "" -render=0 -f pdf -noprc -o $@ $<

dist_noinst_DATA = \
	3S+1D.png \
	B_b3_y.png \
	B_a.png \
	B_b.png \
	crystal.png \
	4S+2D.png \
	4S+2D_reciprocal.png \
	4S+2D_reciprocal2.png \
	4S+2D_reciprocal3.png \
	4S+2D_reciprocal4.png \
	k4cv.png \
	k6c.png \
	zaxis.png \
	e2k_1.png \
	e2k_2.png \
	e2k_3.png \
	e2k_4.png \
	qper_qpar.png

MAINTAINERCLEANFILES = $(dist_noinst_DATA)

EXTRA_DIST = \
	3S+1D.asy \
	B_b3_y.asy \
	B_a.asy \
	B_b.asy \
	crystal.asy \
	4S+2D.asy \
	4S+2D_reciprocal.asy \
	4S+2D_reciprocal2.asy \
	4S+2D_reciprocal3.asy \
	4S+2D_reciprocal4.asy \
	k4cv.asy \
	k6c.asy \
	diffractometer.asy \
	zaxis.asy \
	eulerians.asy \
	e2k_1.asy \
	e2k_2.asy \
	e2k_3.asy \
	e2k_4.asy \
	qper_qpar.asy
