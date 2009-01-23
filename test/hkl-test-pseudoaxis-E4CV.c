#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-e4cv.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis_E4CV

#define SET_AXES(geom, omega, chi, phi, tth) do{\
	hkl_geometry_set_values_v(geom, 4,\
				  omega * HKL_DEGTORAD,\
				  chi * HKL_DEGTORAD,\
				  phi * HKL_DEGTORAD,\
				  tth * HKL_DEGTORAD);\
} while(0)

#define CHECK_PSEUDOAXES(engine, a, b, c) do{				\
		HklParameter *H = (HklParameter *)(engine->pseudoAxes[0]); \
		HklParameter *K = (HklParameter *)(engine->pseudoAxes[1]); \
		HklParameter *L = (HklParameter *)(engine->pseudoAxes[2]); \
									\
		HKL_ASSERT_DOUBLES_EQUAL(a, H->value, HKL_EPSILON); \
		HKL_ASSERT_DOUBLES_EQUAL(b, K->value, HKL_EPSILON); \
		HKL_ASSERT_DOUBLES_EQUAL(c, L->value, HKL_EPSILON); \
	} while(0)

HKL_TEST_SUITE_FUNC(new)
{
	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_e4cv_hkl_new();
	hkl_pseudo_axis_engine_free(engine);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(getter)
{
	HklPseudoAxisEngine *engine;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_e4cv_hkl_new();
	hkl_pseudo_axis_engine_select_get_set(engine, 0);

	// geometry -> pseudo
	SET_AXES(geom, 30., 0., 0., 60.);
	hkl_pseudo_axis_engine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 0., 0., 1.);

	SET_AXES(geom, 30., 0., 90., 60.);
	hkl_pseudo_axis_engine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 1., 0., 0.);

	SET_AXES(geom, 30, 0., -90., 60.);
	hkl_pseudo_axis_engine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, -1., 0., 0.);

	SET_AXES(geom, 30., 0., 180., 60.);
	hkl_pseudo_axis_engine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 0., 0., -1.);

	SET_AXES(geom, 45., 0., 135., 90.);
	hkl_pseudo_axis_engine_getter(engine, geom, &det, sample);
	CHECK_PSEUDOAXES(engine, 1., 0., -1.);

	hkl_pseudo_axis_engine_free(engine);
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

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_e4cv_hkl_new();

	H = &(((HklParameter *)engine->pseudoAxes[0])->value);
	K = &(((HklParameter *)engine->pseudoAxes[1])->value);
	L = &(((HklParameter *)engine->pseudoAxes[2])->value);

	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx){

		hkl_pseudo_axis_engine_select_get_set(engine, f_idx);
		if (engine->getset->parameters_len)
			engine->getset->parameters[0].value = 0.;

		double h, k, l;
		int res;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 0;
		*L = l = 1;

		// pseudo -> geometry
		res = hkl_pseudo_axis_engine_setter(engine, geom, &det, sample);
		//hkl_pseudo_axis_engine_fprintf(stdout, engine);

		// geometry -> pseudo
		if(res == HKL_SUCCESS){
			//hkl_pseudo_axis_engine_fprintf(stdout, engine);
			for(i=0; i<engine->geometries_len; ++i){
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

HKL_TEST_SUITE_FUNC(psi_getter)
{
	HklPseudoAxisEngine *engine = NULL;
	HklGeometry *geom;
	HklDetector detector = {1};
	HklSample *sample;
	size_t i, f_idx;
	double *psi;
	double *h_ref;
	double *k_ref;
	double *l_ref;
	int status;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_e4cv_psi_new();
	hkl_pseudo_axis_engine_select_get_set(engine, 0);

	psi = &(((HklParameter *)engine->pseudoAxes[0])->value);
	h_ref = &engine->getset->parameters[0].value;
	k_ref = &engine->getset->parameters[1].value;
	l_ref = &engine->getset->parameters[2].value;

	// the getter part
	SET_AXES(geom, 30., 0., 0., 60.);
	hkl_pseudo_axis_engine_init(engine, geom, &detector, sample);

	*h_ref = 1;
	*k_ref = 0;
	*l_ref = 0;
	status = hkl_pseudo_axis_engine_getter(engine, geom, &detector, sample);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, status);
	HKL_ASSERT_DOUBLES_EQUAL(0 * HKL_DEGTORAD, *psi, HKL_EPSILON);

	*h_ref = 0;
	*k_ref = 1;
	*l_ref = 0;
	status = hkl_pseudo_axis_engine_getter(engine, geom, &detector, sample);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, status);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, *psi, HKL_EPSILON);

	// here Q and <h, k, l>_ref are colinear
	*h_ref = 0;
	*k_ref = 0;
	*l_ref = 1;
	status = hkl_pseudo_axis_engine_getter(engine, geom, &detector, sample);
	HKL_ASSERT_EQUAL(HKL_FAIL, status);

	*h_ref = -1;
	*k_ref = 0;
	*l_ref = 0;
	status = hkl_pseudo_axis_engine_getter(engine, geom, &detector, sample);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, status);
	HKL_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, *psi, HKL_EPSILON);

	*h_ref = 0;
	*k_ref = -1;
	*l_ref = 0;
	status = hkl_pseudo_axis_engine_getter(engine, geom, &detector, sample);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, status);
	HKL_ASSERT_DOUBLES_EQUAL(-90 * HKL_DEGTORAD, *psi, HKL_EPSILON);
	
	*h_ref = 0;
	*k_ref = 0;
	*l_ref = -1;
	status = hkl_pseudo_axis_engine_getter(engine, geom, &detector, sample);
	HKL_ASSERT_EQUAL(HKL_FAIL, status);

	hkl_pseudo_axis_engine_free(engine);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(psi_setter)
{
	HklPseudoAxisEngine *engine = NULL;
	HklGeometry *geom;
	HklDetector detector = {1};
	HklSample *sample;
	size_t i, f_idx;
	double *Psi;
	double *h_ref, *k_ref, *l_ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	engine = hkl_pseudo_axis_engine_e4cv_psi_new();
	hkl_pseudo_axis_engine_select_get_set(engine, 0);

	Psi = &(((HklParameter *)engine->pseudoAxes[0])->value);
	h_ref = &engine->getset->parameters[0].value;
	k_ref = &engine->getset->parameters[1].value;
	l_ref = &engine->getset->parameters[2].value;

	// the init part
	SET_AXES(geom, 30., 0., 0., 60.);
	*h_ref = 1;
	*k_ref = 0;
	*l_ref = 0;
	hkl_pseudo_axis_engine_init(engine, geom, &detector, sample);


	for(f_idx=0; f_idx<engine->getsets_len; ++f_idx){
		double psi;
		int res;

		hkl_pseudo_axis_engine_select_get_set(engine, f_idx);
		for(psi=-180;psi<180;psi++){
			*Psi = psi * HKL_DEGTORAD;
			
			// pseudo -> geometry
			res = hkl_pseudo_axis_engine_setter(engine, geom, &detector, sample);
			//hkl_pseudo_axis_engine_fprintf(stdout, engine);
			
			// geometry -> pseudo
			if(res == HKL_SUCCESS){
				//hkl_pseudo_axis_engine_fprintf(stdout, engine);
				for(i=0; i<engine->geometries_len; ++i){
					*Psi = 0;
					
					hkl_geometry_init_geometry(engine->geometry, engine->geometries[i]);
					hkl_pseudo_axis_engine_getter(engine, engine->geometry, &detector, sample);
					HKL_ASSERT_DOUBLES_EQUAL(psi * HKL_DEGTORAD, *Psi, HKL_EPSILON);
				}
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
HKL_TEST( getter );
HKL_TEST( degenerated );
HKL_TEST( psi_getter );
HKL_TEST( psi_setter );

HKL_TEST_SUITE_END
