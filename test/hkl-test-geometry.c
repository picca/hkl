#include <math.h>

#include <hkl/hkl-geometry.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME geometry

HKL_TEST_SUITE_FUNC(add_holder)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;

	g = hkl_geometry_new();
	HKL_ASSERT_EQUAL(0, g->holders->len);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "B", 1, 0, 0);
	HKL_ASSERT_EQUAL(1, g->holders->len);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "C", 1, 0, 0);
	HKL_ASSERT_EQUAL(2, g->holders->len);

	HKL_ASSERT_EQUAL(NULL, hkl_geometry_get_holder(g, 2));
	HKL_ASSERT_EQUAL(holder, hkl_geometry_get_holder(g, 1));

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_holder );

HKL_TEST_SUITE_END
