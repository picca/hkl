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

	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_geometry_get_holder(g, 2));
	HKL_ASSERT_POINTER_EQUAL(holder, hkl_geometry_get_holder(g, 1));

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_axis)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	HklAxis *axis0, *axis1, *axis2;

	g = hkl_geometry_new();

	holder = hkl_geometry_add_holder(g);
	axis0 = hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	axis1 = hkl_holder_add_rotation_axis(holder, "B", 1, 0, 0);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	axis2 = hkl_holder_add_rotation_axis(holder, "C", 1, 0, 0);

	HKL_ASSERT_POINTER_EQUAL(axis0, hkl_geometry_get_axis(g, 0));
	HKL_ASSERT_POINTER_EQUAL(axis1, hkl_geometry_get_axis(g, 1));
	HKL_ASSERT_POINTER_EQUAL(axis2, hkl_geometry_get_axis(g, 2));

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(update)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	HklAxis *axis0, *axis1, *axis2;
	HklAxisConfig config;

	g = hkl_geometry_new();

	holder = hkl_geometry_add_holder(g);
	axis0 = hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	axis1 = hkl_holder_add_rotation_axis(holder, "B", 1, 0, 0);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	axis2 = hkl_holder_add_rotation_axis(holder, "C", 1, 0, 0);

	hkl_axis_get_config(axis1, &config);
	config.current = M_PI_2;
	config.consign = -M_PI_2;
	hkl_axis_set_config(axis1, &config);
	// now axis1 is dirty
	HKL_ASSERT_EQUAL(1, axis1->config.dirty);
	
	hkl_geometry_update(g);
	// now axis1 is clean
	HKL_ASSERT_EQUAL(0, axis1->config.dirty);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_holder );
HKL_TEST( get_axis );
HKL_TEST( update );

HKL_TEST_SUITE_END
