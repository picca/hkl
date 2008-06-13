#include <math.h>

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis

#define SET_AXES(geometry, a, b, c, d) do{\
	HklAxis *Omega, *Chi, *Phi, *Tth;\
	HklAxisConfig config;\
	\
	Omega = hkl_geometry_get_axis(geometry, 0);\
	Chi = hkl_geometry_get_axis(geometry, 1);\
	Phi = hkl_geometry_get_axis(geometry, 2);\
	Tth = hkl_geometry_get_axis(geometry, 3);\
	\
	hkl_axis_get_config(Omega, &config);\
	config.value = a * HKL_DEGTORAD;\
	hkl_axis_set_config(Omega, &config);\
	\
	hkl_axis_get_config(Chi, &config);\
	config.value = b * HKL_DEGTORAD;\
	hkl_axis_set_config(Chi, &config);\
	\
	hkl_axis_get_config(Phi, &config);\
	config.value = c * HKL_DEGTORAD;\
	hkl_axis_set_config(Phi, &config);\
	\
	hkl_axis_get_config(Tth, &config);\
	config.value = d * HKL_DEGTORAD;\
	hkl_axis_set_config(Tth, &config);\
} while(0)

#define CHECK_PSEUDOAXES(engine, a, b, c) do{\
	HklPseudoAxis *H, *K, *L;\
	\
	H = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 0);\
	K = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 1);\
	L = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 2);\
	\
	hkl_pseudoAxis_get_config(H, &config);\
	HKL_ASSERT_DOUBLES_EQUAL(a, config.value, HKL_EPSILON);\
	\
	hkl_pseudoAxis_get_config(K, &config);\
	HKL_ASSERT_DOUBLES_EQUAL(b, config.value, HKL_EPSILON);\
	\
	hkl_pseudoAxis_get_config(L, &config);\
	HKL_ASSERT_DOUBLES_EQUAL(c, config.value, HKL_EPSILON);\
	\
} while(0)

HKL_TEST_SUITE_FUNC(new)
{
	HklPseudoAxisEngine *engine = NULL;
	HklPseudoAxisEngineType const *T;

	T = hkl_pseudoAxisEngine_type_auto;
	engine = hkl_pseudoAxisEngine_new(T, "hkl", 3, "h", "k", "l");
	hkl_pseudoAxisEngine_free(engine);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(update)
{
	HklPseudoAxisEngineType const *T;
	HklPseudoAxisEngine *engine;
	HklAxisConfig config;
	HklGeometry *geom;
	HklDetector *det;
	HklSample *sample;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	det = hkl_detector_new();
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	T = hkl_pseudoAxisEngine_type_auto;
	engine = hkl_pseudoAxisEngine_new(T, "hkl", 3, "h", "k", "l");
	hkl_pseudoAxisEngine_set(engine, geom, det, sample, 4, 0, 1, 2, 3);

	// geometry -> pseudo
	SET_AXES(engine->geom, 30., 0., 0., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 0., 0., 1.);

	SET_AXES(engine->geom, 30., 0., 90., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 1., 0., 0.);

	SET_AXES(engine->geom, 30, 0., -90., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, -1., 0., 0.);

	SET_AXES(engine->geom, 30., 0., 180., 60.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 0., 0., -1.);

	SET_AXES(engine->geom, 45., 0., 135., 90.);
	hkl_pseudoAxisEngine_to_pseudoAxes(engine);
	CHECK_PSEUDOAXES(engine, 1., 0., -1.);

	hkl_pseudoAxisEngine_free(engine);
	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklPseudoAxisEngineType const *T;
	HklPseudoAxisEngine *engine = NULL;
	HklPseudoAxis *H, *K, *L;
	HklAxisConfig config;
	HklGeometry *geom;
	HklDetector *det;
	HklSample *sample;
	unsigned int i;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	det = hkl_detector_new();
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	T = hkl_pseudoAxisEngine_type_auto;
	engine = hkl_pseudoAxisEngine_new(T, "hkl", 3, "h", "k", "l");
	hkl_pseudoAxisEngine_set(engine, geom, det, sample, 4, 0, 1, 2, 3);

	H = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 0);
	K = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 1);
	L = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 2);

	for(i=0;i<1000;++i) {
		double h, k, l;
		double hh, kk, ll;
		int res;

		h = (double)rand() / RAND_MAX * 2 - 1.;
		k = (double)rand() / RAND_MAX * 2 - 1.;
		l = (double)rand() / RAND_MAX * 2 - 1.;


		hkl_pseudoAxis_get_config(H, &config);
		config.value = h;
		hkl_pseudoAxis_set_config(H, &config);

		hkl_pseudoAxis_get_config(K, &config);
		config.value = k;
		hkl_pseudoAxis_set_config(K, &config);

		hkl_pseudoAxis_get_config(L, &config);
		config.value = l;
		hkl_pseudoAxis_set_config(L, &config);

		// pseudo -> geometry
		res = hkl_pseudoAxisEngine_to_geometry(engine);

		// geometry -> pseudo
		if (!res) {
			hkl_pseudoAxisEngine_to_pseudoAxes(engine);

			hkl_pseudoAxis_get_config(H, &config);
			hh = config.value;

			hkl_pseudoAxis_get_config(K, &config);
			kk = config.value;

			hkl_pseudoAxis_get_config(L, &config);
			ll = config.value;

			HKL_ASSERT_DOUBLES_EQUAL(h, hh, HKL_EPSILON);
			HKL_ASSERT_DOUBLES_EQUAL(k, kk, HKL_EPSILON);
			HKL_ASSERT_DOUBLES_EQUAL(l, ll, HKL_EPSILON);
		}
	}

	hkl_pseudoAxisEngine_free(engine);
	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(equiv_geometries)
{
	HklPseudoAxisEngineType const *T;
	HklPseudoAxisEngine *engine = NULL;
	HklPseudoAxis *H, *K, *L;
	HklAxisConfig config;
	HklGeometry *geom;
	HklDetector *det;
	HklSample *sample;
	double h, k, l;
	double hh, kk, ll;
	unsigned int i;
	int res;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	det = hkl_detector_new();
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	T = hkl_pseudoAxisEngine_type_auto;
	engine = hkl_pseudoAxisEngine_new(T, "hkl", 3, "h", "k", "l");
	hkl_pseudoAxisEngine_set(engine, geom, det, sample, 4, 0, 1, 2, 3);

	H = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 0);
	K = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 1);
	L = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 2);

	i=0;
	for(i=0;i<1000;++i) {
		h = (double)rand() / RAND_MAX * 2 - 1.;
		k = (double)rand() / RAND_MAX * 2 - 1.;
		l = (double)rand() / RAND_MAX * 2 - 1.;

		hkl_pseudoAxis_get_config(H, &config);
		config.value = h;
		hkl_pseudoAxis_set_config(H, &config);

		hkl_pseudoAxis_get_config(K, &config);
		config.value = k;
		hkl_pseudoAxis_set_config(K, &config);

		hkl_pseudoAxis_get_config(L, &config);
		config.value = l;
		hkl_pseudoAxis_set_config(L, &config);

		// pseudo -> geometry
		res = hkl_pseudoAxisEngine_to_geometry(engine);
		if (!res) {
			hkl_pseudoAxisEngine_to_pseudoAxes(engine);

			hkl_pseudoAxis_get_config(H, &config);
			hh = config.value;

			hkl_pseudoAxis_get_config(K, &config);
			kk = config.value;

			hkl_pseudoAxis_get_config(L, &config);
			ll = config.value;

			HKL_ASSERT_DOUBLES_EQUAL(h, hh, HKL_EPSILON);
			HKL_ASSERT_DOUBLES_EQUAL(k, kk, HKL_EPSILON);
			HKL_ASSERT_DOUBLES_EQUAL(l, ll, HKL_EPSILON);

			hkl_pseudoAxis_get_equiv_geometries(engine);
		}
	}

	hkl_pseudoAxisEngine_free(engine);
	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( update );
HKL_TEST( set );
HKL_TEST( equiv_geometries );

HKL_TEST_SUITE_END
