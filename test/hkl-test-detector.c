#include <math.h>

#include <hkl/hkl-detector.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME detector

HKL_TEST_SUITE_FUNC(compute_kf)
{
	HklDetector det = {0};
	HklGeometry *geom = NULL;
	HklAxis *axis1 = NULL;
	HklAxis *axis2 = NULL;
	HklHolder *holder = NULL;
	HklVector kf;
	HklVector kf_ref = {{0, HKL_TAU / HKL_SOURCE_DEFAULT_WAVE_LENGTH, 0}};

	geom = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "b", 0, 1, 0);

	hkl_axis_set_value(&geom->axes[0], M_PI_2);
	hkl_axis_set_value(&geom->axes[1], M_PI_2);

	hkl_detector_compute_kf(&det, geom, &kf);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&kf_ref, &kf));

	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( compute_kf );

HKL_TEST_SUITE_END
