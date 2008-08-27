#include <string.h>
#include <math.h>

#include <hkl/hkl-geometry.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME holder

HKL_TEST_SUITE_FUNC(add_rotation_axis)
{
	HklAxis *axis = NULL;
	HklGeometry *geom = NULL;
	HklHolder holder;

	geom = hkl_geometry_new();
	hkl_holder_init(&holder, geom);

	// add two different axis
	axis = hkl_holder_add_rotation_axis(&holder, "a", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	HKL_ASSERT_EQUAL(1, holder.axes_len);
	axis = hkl_holder_add_rotation_axis(&holder, "b", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	HKL_ASSERT_EQUAL(2, holder.axes_len);

	// can not add two times the same axes, must return the same axis
	axis = hkl_holder_add_rotation_axis(&holder, "a", 1, 0, 0);
	HKL_ASSERT_POINTER_EQUAL(NULL, axis);
	HKL_ASSERT_EQUAL(2, holder.axes_len);

	// release the axes memory as holder do not manage it.
	hkl_geometry_free(geom);

	hkl_holder_release_memory(&holder);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_rotation_axis );

HKL_TEST_SUITE_END
