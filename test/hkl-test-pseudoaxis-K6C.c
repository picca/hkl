#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-common-eulerians.h>
#include <hkl/hkl-pseudoaxis-k6c.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis_K6C

#define SET_AXES(geometry, mu, komega, kappa, kphi, gamma, delta) do{\
	hkl_geometry_set_values_v(geometry, 6,\
				  mu * HKL_DEGTORAD,\
				  komega * HKL_DEGTORAD,\
				  kappa * HKL_DEGTORAD,\
				  kphi * HKL_DEGTORAD,\
				  gamma * HKL_DEGTORAD,\
				  delta * HKL_DEGTORAD);\
} while(0)

HKL_TEST_SUITE_FUNC(new)
{
	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_k6c_hkl_new();
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

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_k6c_hkl_new();

	H = &(((HklParameter *)engine->pseudoAxes[0])->value);
	K = &(((HklParameter *)engine->pseudoAxes[1])->value);
	L = &(((HklParameter *)engine->pseudoAxes[2])->value);

	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx) {
		hkl_pseudo_axis_engine_select_get_set(engine, f_idx);
		if (engine->getset->parameters_len)
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
			//hkl_pseudo_axis_engine_fprintf(stdout, engine);
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

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_eulerians_new();

	Omega = &(((HklParameter *)engine->pseudoAxes[0])->value);
	Chi   = &(((HklParameter *)engine->pseudoAxes[1])->value);
	Phi   = &(((HklParameter *)engine->pseudoAxes[2])->value);

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

HKL_TEST_SUITE_FUNC(manip)
{
	HklPseudoAxisEngineList *engines = NULL;
	HklPseudoAxisEngine *hkl = NULL;
	HklPseudoAxisEngine *psi = NULL;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, f_idx;
	double *H, *K, *L;
	double H2, K2, L2;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	hkl_source_init(&geom->source, 2.0837, 1., 0., 0.);
	hkl_source_fprintf(stdout, &geom->source);

	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	//hkl_matrix_from_euler(&sample->U, -90 * HKL_DEGTORAD, 0., 0.);
	hkl_sample_set_lattice(sample,
			       2.88, 2.88, 2.88,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	hkl_matrix_from_euler(&sample->U, -90 * HKL_DEGTORAD, 0., 0.);
	hkl_sample_fprintf(stdout, sample);

	engines = hkl_pseudo_axis_engine_list_new();
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_k6c_hkl_new());
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_eulerians_new());
	hkl_pseudo_axis_engine_list_add(engines, hkl_pseudo_axis_engine_k6c_psi_new());

	hkl = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");
	psi = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	H = &(((HklParameter *)hkl->pseudoAxes[0])->value);
	K = &(((HklParameter *)hkl->pseudoAxes[1])->value);
	L = &(((HklParameter *)hkl->pseudoAxes[2])->value);

	hkl_pseudo_axis_engine_select_get_set(hkl, 9);

	*H = 0;
	*K = 0;
	*L = 1. - 0.047;
	H2 = hkl->getset->parameters[0].value = 0;
	K2 = hkl->getset->parameters[1].value = 1.;
	L2 = hkl->getset->parameters[2].value = 1. - 2*0.047;
	if( HKL_SUCCESS == hkl_pseudo_axis_engine_setter(hkl, geom, &det, sample)){
			for(i=0; i<hkl->geometries_len; ++i) {
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(geom, hkl->geometries[i]);
				hkl_pseudo_axis_engine_init(psi, geom, &det, sample);
				hkl_pseudo_axis_engine_list_getter(engines, geom, &det, sample);
				hkl_pseudo_axis_engine_list_fprintf(stdout, engines);
			}
	}

	hkl_geometry_init_geometry(geom, hkl->geometries[0]);
	hkl_pseudo_axis_engine_select_get_set(hkl, 7);
	*H = H2;
	*K = K2;
	*L = L2;
	fprintf(stdout, "coucou\n");
	if (HKL_SUCCESS == hkl_pseudo_axis_engine_setter(hkl, geom, &det, sample)){
		fprintf(stdout, "coucou\n");
		for(i=0; i<hkl->geometries_len; ++i){
			*H = *K = *L = 0;
			hkl_geometry_init_geometry(geom, hkl->geometries[i]);
			hkl_pseudo_axis_engine_list_getter(engines, geom, &det, sample);
			hkl_pseudo_axis_engine_list_fprintf(stdout, engines);
		}
	}
	hkl_pseudo_axis_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( degenerated );
HKL_TEST( eulerians );
//HKL_TEST( manip );

HKL_TEST_SUITE_END
