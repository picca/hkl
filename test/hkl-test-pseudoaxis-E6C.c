#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-e6c-hkl.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis_E6C

#define SET_AXES(geometry, a, b, c, d, e, f) do{\
	HklAxisConfig *Mu = &geometry->axes[0]->config;\
	HklAxisConfig *Omega = &geometry->axes[1]->config;\
	HklAxisConfig *Chi = &geometry->axes[2]->config;\
	HklAxisConfig *Phi = &geometry->axes[3]->config;\
	HklAxisConfig *Gamma = &geometry->axes[4]->config;\
	HklAxisConfig *Delta = &geometry->axes[5]->config;\
	\
	Mu->value = a * HKL_DEGTORAD;\
	Mu->dirty = 1;\
	\
	Omega->value = b * HKL_DEGTORAD;\
	Omega->dirty = 1;\
	\
	Chi->value = c * HKL_DEGTORAD;\
	Chi->dirty = 1;\
	\
	Phi->value = d * HKL_DEGTORAD;\
	Phi->dirty = 1;\
	\
	Gamma->value = e * HKL_DEGTORAD;\
	Gamma->dirty = 1;\
	\
	Delta->value = f * HKL_DEGTORAD;\
	Delta->dirty = 1;\
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
	HklPseudoAxisEngine *engine = hkl_pseudoAxisEngine_new_E6C_HKL();
	hkl_pseudoAxisEngine_free(engine);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(getter)
{
	HklPseudoAxisEngine *engine;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN6C);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudoAxisEngine_new_E6C_HKL();
	hkl_pseudoAxisEngine_select_get_set(engine, 0);

	// geometry -> pseudo
	SET_AXES(geom, 0., 30., 0., 0., 0., 60.);
	hkl_pseudoAxisEngine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 0., 0., 1.);

	SET_AXES(geom, 0., 30., 0., 90., 0., 60.);
	hkl_pseudoAxisEngine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 1., 0., 0.);

	SET_AXES(geom, 0., 30, 0., -90., 0., 60.);
	hkl_pseudoAxisEngine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, -1., 0., 0.);

	SET_AXES(geom, 0., 30., 0., 180., 0., 60.);
	hkl_pseudoAxisEngine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 0., 0., -1.);

	SET_AXES(geom, 0., 45., 0., 135., 0., 90.);
	hkl_pseudoAxisEngine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 1., 0., -1.);

	hkl_pseudoAxisEngine_free(engine);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(degenerated)
{
	HklPseudoAxisEngine *engine = NULL;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, f_idx;
	double *H, *K, *L;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN6C);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudoAxisEngine_new_E6C_HKL();

	H = &engine->pseudoAxes[0].config.value;
	K = &engine->pseudoAxes[1].config.value;
	L = &engine->pseudoAxes[2].config.value;

	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudoAxisEngine_select_get_set(engine, f_idx);
		if (engine->getset->parameters_len)
			engine->getset->parameters[0].value = 0.;

		double h, k, l;
		int res;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 0;
		*L = l = 1;

		// pseudo -> geometry
		res = hkl_pseudoAxisEngine_setter(engine, geom, &det, sample);
		//hkl_pseudoAxisEngine_fprintf(stdout, engine);

		// geometry -> pseudo
		if (res == HKL_SUCCESS) {
			//hkl_pseudoAxisEngine_fprintf(stdout, engine);
			for(i=0; i<engine->geometries_len; ++i) {
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(engine->geometry, engine->geometries[i]);
				hkl_pseudoAxisEngine_getter(engine, engine->geometry, &det, sample);

				HKL_ASSERT_DOUBLES_EQUAL(h, *H, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(k, *K, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(l, *L, HKL_EPSILON);
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
HKL_TEST( getter );
HKL_TEST( degenerated );

HKL_TEST_SUITE_END
