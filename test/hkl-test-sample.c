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

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( add_reflection );
HKL_TEST( get_reflection );
HKL_TEST( del_reflection );

HKL_TEST_SUITE_END
