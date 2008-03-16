#include <math.h>

#include <hkl/hkl-sample.h>
#include <hkl/hkl-geometry-factory.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME sample

HKL_TEST_SUITE_FUNC(new)
{
	HklDetector *det;
	HklGeometry *geom;
	HklSample *sample;

	det = hkl_detector_new();
	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(add_reflection)
{
	HklDetector *det;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	det = hkl_detector_new();
	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, det, 1, 0, 0);

	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_reflection)
{
	HklDetector *det;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklSampleReflection *ref2;

	det = hkl_detector_new();
	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, det, 1, 0, 0);
	ref2 = hkl_sample_get_reflection(sample, 0);
	HKL_ASSERT_EQUAL(ref, ref2);

	ref = hkl_sample_add_reflection(sample, geom, det, -1, 0, 0);
	ref = hkl_sample_add_reflection(sample, geom, det, 0, 1, 0);

	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(del_reflection)
{
	HklDetector *det;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	det = hkl_detector_new();
	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, det, 1, 0, 0);
	hkl_sample_del_reflection(sample, 0);
	HKL_ASSERT_EQUAL(0, sample->reflections->len);

	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(compute_UB_busing_levy)
{
	HklDetector *det;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);

#define SET_ANGLES(a, b, c, d)\
do {\
	HklAxis *Omega, *Chi, *Phi, *Tth;\
	HklAxisConfig omega, chi, phi, tth;\
\
	Omega = hkl_geometry_get_axis(geom, 0);\
	Chi = hkl_geometry_get_axis(geom, 1);\
	Phi = hkl_geometry_get_axis(geom, 2);\
	Tth = hkl_geometry_get_axis(geom, 3);\
	hkl_axis_get_config(Omega, &omega);\
	hkl_axis_get_config(Chi, &chi);\
	hkl_axis_get_config(Phi, &phi);\
	hkl_axis_get_config(Tth, &tth);\
\
	omega.current = a * HKL_DEGTORAD;\
	chi.current = b * HKL_DEGTORAD;\
	phi.current = c * HKL_DEGTORAD;\
	tth.current = d * HKL_DEGTORAD;\
\
	hkl_axis_set_config(Omega, &omega);\
	hkl_axis_set_config(Chi, &chi);\
	hkl_axis_set_config(Phi, &phi);\
	hkl_axis_set_config(Tth, &tth);\
}while(0)
	det = hkl_detector_new();
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	SET_ANGLES(30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, det, 0, 0, 1);

	SET_ANGLES(30, 0, -90, 60);
	ref = hkl_sample_add_reflection(sample, geom, det, -1, 0, 0);

	hkl_sample_compute_UB_busing_levy(sample, 0, 1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_I, sample->U));

	SET_ANGLES(30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, det, 1, 0, 0);

	SET_ANGLES(30, 0, 180, 60);
	ref = hkl_sample_add_reflection(sample, geom, det, 0, 1, 0);

	hkl_sample_compute_UB_busing_levy(sample, 2, 3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, sample->U));

	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

#undef SET_ANGLES
	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(affine)
{
	HklDetector *det;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);

#define SET_ANGLES(a, b, c, d)\
do {\
	HklAxis *Omega, *Chi, *Phi, *Tth;\
	HklAxisConfig omega, chi, phi, tth;\
\
	Omega = hkl_geometry_get_axis(geom, 0);\
	Chi = hkl_geometry_get_axis(geom, 1);\
	Phi = hkl_geometry_get_axis(geom, 2);\
	Tth = hkl_geometry_get_axis(geom, 3);\
	hkl_axis_get_config(Omega, &omega);\
	hkl_axis_get_config(Chi, &chi);\
	hkl_axis_get_config(Phi, &phi);\
	hkl_axis_get_config(Tth, &tth);\
\
	omega.current = a * HKL_DEGTORAD;\
	chi.current = b * HKL_DEGTORAD;\
	phi.current = c * HKL_DEGTORAD;\
	tth.current = d * HKL_DEGTORAD;\
\
	hkl_axis_set_config(Omega, &omega);\
	hkl_axis_set_config(Chi, &chi);\
	hkl_axis_set_config(Phi, &phi);\
	hkl_axis_set_config(Tth, &tth);\
}while(0)
	det = hkl_detector_new();
	det->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	SET_ANGLES(30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, det, 1, 0, 0);

	SET_ANGLES(30, 0, 180, 60);
	ref = hkl_sample_add_reflection(sample, geom, det, 0, 1, 0);

	hkl_sample_affine(sample);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, sample->U));

	hkl_sample_free(sample);
	hkl_detector_free(det);
	hkl_geometry_free(geom);

#undef SET_ANGLES
	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( add_reflection );
HKL_TEST( get_reflection );
HKL_TEST( del_reflection );
HKL_TEST( compute_UB_busing_levy );
HKL_TEST( affine );

HKL_TEST_SUITE_END
