TESTS_ENVIRONMENT=env GI_TYPELIB_PATH=$(top_builddir)/hkl $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la
TESTS =\
	source/bindings/python.py \
	source/pyplots/trajectory_full.py \
	source/pyplots/trajectory_simple.py

EXTRA_DIST = \
	source/conf.py.in \
	source/bindings/bindings.rst \
	source/development.rst \
	source/diffractometers/e4cv.rst \
	source/diffractometers/e6c.rst \
	source/diffractometers/k4cv.rst \
	source/diffractometers/k6c.rst \
	source/diffractometers/mars.rst \
	source/diffractometers/med1_2.rst \
	source/diffractometers/med2_2.rst \
	source/diffractometers/med2_3.rst \
	source/diffractometers/petra3.rst \
	source/diffractometers/soleil_sirius_kappa.rst \
	source/diffractometers/soleil_sirius_turret.rst \
	source/diffractometers/zaxis.rst \
	source/index.rst \
	source/introduction.rst \
	source/pseudo.rst \
	$(TESTS)

-include $(top_srcdir)/sphinx.make

publish: html
	rsync -avz build/html/ picca@people.debian.org:public_html/hkl
