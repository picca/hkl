#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-common-eulerians.h>
#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-e6c.h>
#include <hkl/hkl-pseudoaxis-k4cv.h>
#include <hkl/hkl-pseudoaxis-k6c.h>

#include "hkl-test.h"

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
	double values[engine->pseudoAxes_len];
	int miss = 0;

	// randomize the geometry
	hkl_geometry_randomize(geometry);
	
	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudo_axis_engine_select_get_set(engine, f_idx);
		miss = 0;
		for(i=0;i<100;++i) {
			int res;

			// randomize the pseudoAxes values
			for(j=0; j<engine->pseudoAxes_len; ++j) {
				HklAxisConfig *config;

				config = &engine->pseudoAxes[j].config;
				hkl_axis_config_randomize(config);
				values[j] = config->value;
			}

			// randomize the parameters
			for(j=0; j<engine->getset->parameters_len; ++j)
				hkl_parameter_randomize(&engine->getset->parameters[j]);

			// pseudo -> geometry
			hkl_pseudo_axis_engine_init(engine, geometry, &det, sample);
			res = hkl_pseudo_axis_engine_setter(engine, geometry, &det, sample);
			//hkl_pseudo_axis_engine_fprintf(stdout, engine);

			// geometry -> pseudo
			if (res == HKL_SUCCESS) {
				// check all finded geometries
				for(j=0; j<engine->geometries_len; ++j) {
					// first modify the pseudoAxes values
					// to be sure that the result is the
					// computed result.
					for(k=0; k<engine->pseudoAxes_len; ++k)
						engine->pseudoAxes[k].config.value = 0.;

					hkl_geometry_init_geometry(engine->geometry, engine->geometries[j]);
					hkl_pseudo_axis_engine_getter(engine, engine->geometry, &det, sample);

					for(k=0; k<engine->pseudoAxes_len; ++k) {
						HKL_ASSERT_DOUBLES_EQUAL(values[k],
								engine->pseudoAxes[k].config.value,
								HKL_EPSILON);
					}
				}
			} else
				miss++;
		}
		fprintf(stderr, "\n\"%s\" \"%s\" missed : %d",
				engine->geometry->name,
				engine->getset->name, miss);
	}
	fprintf(stderr, "\n");

	return HKL_TEST_PASS;
}

static test_engines(struct hkl_test *test,
		    HklPseudoAxisEngineList *engines,
		    HklGeometry *geometry, HklSample *sample)
{
	size_t i;
	for(i=0; i<engines->engines_len; ++i)
		test_engine(test, engines->engines[i], geometry, sample);
}

HKL_TEST_SUITE_FUNC(set)
{
	HklGeometry *geometry = NULL;
	HklSample *sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	HklPseudoAxisEngineList *engines = hkl_pseudo_axis_engine_list_new();

	// test all E4CV engines
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_e4cv_hkl_new());
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_e4cv_psi_new());
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);

	// test all E6C HKL engines
	hkl_pseudo_axis_engine_list_clear(engines);
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_e6c_hkl_new());
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_e6c_psi_new());
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN6C);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);

	// test all K4CV HKL engines
	hkl_pseudo_axis_engine_list_clear(engines);
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_k4cv_hkl_new());
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_k4cv_psi_new());
	//hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_eulerians_new());
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA4C_VERTICAL, 50 * HKL_DEGTORAD);
	test_engines(test, engines, geometry, sample);
	hkl_geometry_free(geometry);

	// test all K6C engines
	hkl_pseudo_axis_engine_list_clear(engines);
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_k6c_hkl_new());
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_k6c_psi_new());
	//hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_eulerians_new());
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
