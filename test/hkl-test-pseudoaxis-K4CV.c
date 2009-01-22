#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-common-eulerians.h>
#include <hkl/hkl-pseudoaxis-k4cv.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis_K4CV

#define SET_AXES(geometry, komega, kappa, kphi, tth) do{\
	hkl_geometry_set_values_v(geometry, 4,\
				  komega * HKL_DEGTORAD,\
				  kappa * HKL_DEGTORAD,\
				  kphi * HKL_DEGTORAD,\
				  tth * HKL_DEGTORAD);\
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
	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_k4cv_hkl_new();
	hkl_pseudo_axis_engine_free(engine);

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

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA4C_VERTICAL, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_k4cv_hkl_new();

	H = &engine->pseudoAxes[0].config.value;
	K = &engine->pseudoAxes[1].config.value;
	L = &engine->pseudoAxes[2].config.value;

	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudo_axis_engine_select_get_set(engine, f_idx);
		if (f_idx>0)
			engine->getset->parameters[0].value = 1.;

		double h, k, l;
		int res;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 1;
		*L = l = 0;

		// pseudo -> geometry
		res = hkl_pseudo_axis_engine_setter(engine, geom, &det, sample);
		//hkl_pseudo_axis_engine_fprintf(stdout, engine);

		// geometry -> pseudo
		if (res == HKL_SUCCESS) {
			for(i=0; i<engine->geometries_len; ++i) {
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(engine->geometry, engine->geometries[i]);
				hkl_pseudo_axis_engine_getter(engine, engine->geometry, &det, sample);

				HKL_ASSERT_DOUBLES_EQUAL(h, *H, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(k, *K, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(l, *L, HKL_EPSILON);
			}
		}
	}

	hkl_pseudo_axis_engine_free(engine);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(eulerians)
{
	HklPseudoAxisEngine *engine = NULL;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, f_idx;
	double *Omega, *Chi, *Phi;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA4C_VERTICAL, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_eulerians_new();

	Omega = &engine->pseudoAxes[0].config.value;
	Chi = &engine->pseudoAxes[1].config.value;
	Phi = &engine->pseudoAxes[2].config.value;

	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudo_axis_engine_select_get_set(engine, f_idx);
		if (f_idx>0)
			engine->getset->parameters[0].value = 1.;

		double omega, chi, phi;
		int res;

		/* studdy this degenerated case */
		*Omega = omega = 0;
		*Chi = chi = 90 * HKL_DEGTORAD;
		*Phi = phi = 0;

		// pseudo -> geometry
		res = hkl_pseudo_axis_engine_setter(engine, geom, &det, sample);
		//hkl_pseudo_axis_engine_fprintf(stdout, engine);

		// geometry -> pseudo
		if (res == HKL_SUCCESS) {
			for(i=0; i<engine->geometries_len; ++i) {
				*Omega = *Chi = *Phi = 0;

				hkl_geometry_init_geometry(engine->geometry, engine->geometries[i]);
				hkl_pseudo_axis_engine_getter(engine, engine->geometry, &det, sample);
				//hkl_pseudo_axis_engine_fprintf(stdout, engine);

				HKL_ASSERT_DOUBLES_EQUAL(omega, *Omega, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(chi, *Chi, HKL_EPSILON);
				HKL_ASSERT_DOUBLES_EQUAL(phi, *Phi, HKL_EPSILON);
			}
		}
	}

	hkl_pseudo_axis_engine_free(engine);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}
HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( degenerated );
HKL_TEST( eulerians );

HKL_TEST_SUITE_END
