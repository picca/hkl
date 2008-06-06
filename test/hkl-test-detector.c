#include <math.h>

#include <hkl/hkl-detector.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME detector

HKL_TEST_SUITE_FUNC(new)
{
	HklDetector *det = NULL;
	HklGeometry *geom = NULL;
	HklAxis *axis1 = NULL;
	HklAxis *axis2 = NULL;
	HklHolder *holder = NULL;

	geom = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geom);
	axis1 = hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	axis2 = hkl_holder_add_rotation_axis(holder, "b", 0, 1, 0);

	det = hkl_detector_new();

	HKL_ASSERT_EQUAL(0, det->idx);

	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_kf)
{
	HklDetector *det = NULL;
	HklGeometry *geom = NULL;
	HklAxis *axis1 = NULL;
	HklAxis *axis2 = NULL;
	HklHolder *holder = NULL;
	HklVector kf;
	HklVector kf_ref = {{0, HKL_TAU / HKL_SOURCE_DEFAULT_WAVE_LENGTH, 0}};
	HklAxisConfig config;

	geom = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geom);
	axis1 = hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	axis2 = hkl_holder_add_rotation_axis(holder, "b", 0, 1, 0);

	det = hkl_detector_new();

	hkl_axis_get_config(axis1, &config);
	config.value = M_PI_2;
	hkl_axis_set_config(axis1, &config);
	hkl_axis_set_config(axis2, &config);

	hkl_detector_get_kf(det, geom, &kf);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&kf_ref, &kf));

	hkl_detector_free(det);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( get_kf );

HKL_TEST_SUITE_END
