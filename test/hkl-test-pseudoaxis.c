#include <math.h>

#include "hkl-test.h"
#include <hkl/hkl-pseudoaxis-factory.h>

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis

static int test_engine(struct hkl_test *test,
		       HklPseudoAxisEngine *engine,
		       HklGeometry *geometry, HklSample *sample)
{
	HklDetector det = {1};
	size_t i, j, k, f_idx;
	double values[HKL_LIST_LEN(engine->pseudoAxes)];
	int miss = 0;

	// randomize the geometry
	hkl_geometry_randomize(geometry);
	
	for(f_idx=0; f_idx<HKL_LIST_LEN(engine->modes); ++f_idx) {
		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		miss = 0;
		for(i=0;i<100;++i) {
			int res;

			// randomize the pseudoAxes values
			for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j) {
				HklParameter *parameter = (HklParameter *)(engine->pseudoAxes[j]);
				hkl_parameter_randomize(parameter);

				values[j] = parameter->value;
			}

			// randomize the parameters
			for(j=0; j<HKL_LIST_LEN(engine->mode->parameters); ++j)
				hkl_parameter_randomize(&engine->mode->parameters[j]);

			// pseudo -> geometry
			hkl_pseudo_axis_engine_init(engine, geometry, &det, sample);
			res = hkl_pseudo_axis_engine_setter(engine, geometry, &det, sample);
			//hkl_pseudo_axis_engine_fprintf(stdout, engine);

			// geometry -> pseudo
			if (res == HKL_SUCCESS) {
				// check all finded geometries
				for(j=0; j<engine->geometries->len; ++j) {
					// first modify the pseudoAxes values
					// to be sure that the result is the
					// computed result.
					size_t len;

					len = HKL_LIST_LEN(engine->pseudoAxes);
					for(k=0; k<len; ++k)
						((HklParameter *)engine->pseudoAxes[k])->value = 0.;

					hkl_geometry_init_geometry(engine->geometry, engine->geometries->geometries[j]);
					hkl_pseudo_axis_engine_getter(engine, engine->geometry, &det, sample);

					for(k=0; k<len; ++k) {
						HKL_ASSERT_DOUBLES_EQUAL(values[k],
									 ((HklParameter *)engine->pseudoAxes[k])->value,
								HKL_EPSILON);
					}
				}
			} else
				miss++;
		}
		fprintf(stderr, "\n\"%s\" \"%s\" missed : %d",
				engine->geometry->name,
				engine->mode->name, miss);
	}
	fprintf(stderr, "\n");

	return HKL_TEST_PASS;
}

static test_engines(struct hkl_test *test,
		    HklPseudoAxisEngineList *engines,
		    HklGeometry *geometry, HklSample *sample)
{
	size_t i;
	for(i=0; i<HKL_LIST_LEN(engines->engines); ++i)
		test_engine(test, engines->engines[i], geometry, sample);
}

HKL_TEST_SUITE_FUNC(set)
{
	HklGeometry *geometry = NULL;
	HklSample *sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	HklPseudoAxisEngineList *engines;

	// test all E4CV engines
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all E6C HKL engines
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_EULERIAN6C);
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN6C);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all K4CV HKL engines
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_KAPPA4C_VERTICAL);
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA4C_VERTICAL, 50 * HKL_DEGTORAD);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all K6C engines
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_KAPPA6C);
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	hkl_sample_free(sample);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( set );

HKL_TEST_SUITE_END
