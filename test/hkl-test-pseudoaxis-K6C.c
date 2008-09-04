#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-K6C.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis_K6C

HKL_TEST_SUITE_FUNC(new)
{
	HklPseudoAxisEngine *engine = hkl_pseudoAxisEngine_new_K6C_HKL();
	hkl_pseudoAxisEngine_free(engine);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklPseudoAxisEngine *engine = NULL;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, j, f_idx;
	double *H, *K, *L;
	int miss = 0;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudoAxisEngine_new_K6C_HKL();

	H = &engine->pseudoAxes[0].config.value;
	K = &engine->pseudoAxes[1].config.value;
	L = &engine->pseudoAxes[2].config.value;

	geom->axes[1]->config.value = 90 * HKL_DEGTORAD;
	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudoAxisEngine_select_get_set(engine, f_idx);
		if (f_idx>0)
			engine->getset->parameters[0].value = 1.;
		miss = 0;
		for(i=0;i<1000;++i) {
			double h, k, l;
			int res;

			*H = h = (double)rand() / RAND_MAX * 2 - 1.;
			*K = k = (double)rand() / RAND_MAX * 2 - 1.;
			*L = l = (double)rand() / RAND_MAX * 2 - 1.;

			// pseudo -> geometry
			res = hkl_pseudoAxisEngine_setter(engine, geom, &det, sample);

			//hkl_pseudoAxisEngine_fprintf(engine, stdout);

			// geometry -> pseudo
			if (res == HKL_SUCCESS) {
				for(j=0; j<engine->geometries_len; ++j) {
					*H = *K = *L = 0;

					hkl_geometry_init_geometry(engine->geometry, engine->geometries[j]);
					hkl_pseudoAxisEngine_getter(engine, engine->geometry, &det, sample);

					HKL_ASSERT_DOUBLES_EQUAL(h, *H, HKL_EPSILON);
					HKL_ASSERT_DOUBLES_EQUAL(k, *K, HKL_EPSILON);
					HKL_ASSERT_DOUBLES_EQUAL(l, *L, HKL_EPSILON);
				}
			} else
				miss++;
		}
		fprintf(stderr, "\nK6C  \"%s\" missed : %d", engine->getset->name, miss);
	}

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

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudoAxisEngine_new_K6C_HKL();

	H = &engine->pseudoAxes[0].config.value;
	K = &engine->pseudoAxes[1].config.value;
	L = &engine->pseudoAxes[2].config.value;

	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudoAxisEngine_select_get_set(engine, f_idx);
		if (f_idx>0)
			engine->getset->parameters[0].value = 1.;

		double h, k, l;
		int res;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 1;
		*L = l = 0;

		// pseudo -> geometry
		res = hkl_pseudoAxisEngine_setter(engine, geom, &det, sample);
		//hkl_pseudoAxisEngine_fprintf(engine, stdout);

		// geometry -> pseudo
		if (res) {
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
HKL_TEST( set );
HKL_TEST( degenerated );

HKL_TEST_SUITE_END
