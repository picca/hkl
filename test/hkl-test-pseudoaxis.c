#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-E4CV.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis

#define SET_AXES(geometry, a, b, c, d) do{\
	HklAxisConfig *Omega = &geometry->axes[0]->config;\
	HklAxisConfig *Chi = &geometry->axes[1]->config;\
	HklAxisConfig *Phi = &geometry->axes[2]->config;\
	HklAxisConfig *Tth = &geometry->axes[3]->config;\
	\
	Omega->value = a * HKL_DEGTORAD;\
	Omega->dirty = 1;\
	\
	Chi->value = b * HKL_DEGTORAD;\
	Chi->dirty = 1;\
	\
	Phi->value = c * HKL_DEGTORAD;\
	Phi->dirty = 1;\
	\
	Tth->value = d * HKL_DEGTORAD;\
	Tth->dirty = 1;\
} while(0)

#define CHECK_PSEUDOAXES(engine, a, b, c) do{\
	HklPseudoAxis *H = &engine->pseudoAxes[0];\
	HklPseudoAxis *K = &engine->pseudoAxes[1];\
	HklPseudoAxis *L = &engine->pseudoAxes[2];\
	\
	HKL_ASSERT_DOUBLES_EQUAL(a, H->config.value, HKL_EPSILON);\
	HKL_ASSERT_DOUBLES_EQUAL(b, K->config.value, HKL_EPSILON);\
	HKL_ASSERT_DOUBLES_EQUAL(c, L->config.value, HKL_EPSILON);\
} while(0)

HKL_TEST_SUITE_FUNC(new)
{
	HklPseudoAxisEngine *engine = hkl_pseudoAxisEngine_new_E4CV_HKL();
	hkl_pseudoAxisEngine_free(engine);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(update)
{
	HklPseudoAxisEngine *engine;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudoAxisEngine_new_E4CV_HKL();
	hkl_pseudoAxisEngine_set(engine, 0, geom, &det, sample);

	// geometry -> pseudo
	SET_AXES(engine->geometry, 30., 0., 0., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 0., 0., 1.);

	SET_AXES(engine->geometry, 30., 0., 90., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 1., 0., 0.);

	SET_AXES(engine->geometry, 30, 0., -90., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, -1., 0., 0.);

	SET_AXES(engine->geometry, 30., 0., 180., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 0., 0., -1.);

	SET_AXES(engine->geometry, 45., 0., 135., 90.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 1., 0., -1.);

	hkl_pseudoAxisEngine_free(engine);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklPseudoAxisEngine *engine = NULL;
	HklPseudoAxis *H, *K, *L;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, j;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudoAxisEngine_new_E4CV_HKL();
	hkl_pseudoAxisEngine_set(engine, 0, geom, &det, sample);

	H = &engine->pseudoAxes[0];
	K = &engine->pseudoAxes[1];
	L = &engine->pseudoAxes[2];

	for(i=0;i<1000;++i) {
		double h, k, l;
		int res;

		h = (double)rand() / RAND_MAX * 2 - 1.;
		k = (double)rand() / RAND_MAX * 2 - 1.;
		l = (double)rand() / RAND_MAX * 2 - 1.;


		H->config.value = h;
		K->config.value = k;
		L->config.value = l;

		// pseudo -> geometry
		res = hkl_pseudoAxisEngine_to_geometry(engine);

		// geometry -> pseudo
		if (res) {
			for(j=0; j<engine->geometries_len; ++j) {
				H->config.value = 0;
				K->config.value = 0;
				L->config.value = 0;

				hkl_geometry_init_geometry(engine->geometry, engine->geometries[j]);
				hkl_pseudoAxisEngine_to_pseudoAxes(engine);

				HKL_ASSERT_DOUBLES_EQUAL(h, H->config.value, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(k, K->config.value, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(l, L->config.value, HKL_EPSILON);
			}
		}
	}

	hkl_pseudoAxisEngine_free(engine);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( update );
HKL_TEST( set );

HKL_TEST_SUITE_END
