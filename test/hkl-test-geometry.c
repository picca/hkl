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
	HKL_ASSERT_EQUAL(0, g->holders_len);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	HKL_ASSERT_EQUAL(1, g->holders_len);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);
	HKL_ASSERT_EQUAL(2, g->holders_len);

	HKL_ASSERT_POINTER_EQUAL(holder, &g->holders[1]);

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
	axis0 = hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	axis1 = hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	axis2 = hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	HKL_ASSERT_POINTER_EQUAL(axis0, g->axes[0]);
	HKL_ASSERT_POINTER_EQUAL(axis1, g->axes[1]);
	HKL_ASSERT_POINTER_EQUAL(axis2, g->axes[2]);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(update)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	HklAxis *axis0, *axis1, *axis2;

	g = hkl_geometry_new();

	holder = hkl_geometry_add_holder(g);
	axis0 = hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	axis1 = hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	axis2 = hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	hkl_parameter_set_value((HklParameter *)axis1, M_PI_2);
	// now axis1 is dirty
	HKL_ASSERT_EQUAL(HKL_TRUE, ((HklParameter *)axis1)->changed);
	
	hkl_geometry_update(g);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2), g->holders[0].q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2), g->holders[0].q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, g->holders[0].q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, g->holders[0].q.data[3], HKL_EPSILON);
	// now axis1 is clean
	HKL_ASSERT_EQUAL(HKL_FALSE, ((HklParameter *)axis1)->changed);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set_values)
{
	HklGeometry *g;
	HklHolder *holder;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	hkl_geometry_set_values_v(g, 3, 1., 1., 1.);
	HKL_ASSERT_DOUBLES_EQUAL(1., ((HklParameter *)(g->axes[0]))->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., ((HklParameter *)(g->axes[1]))->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., ((HklParameter *)(g->axes[2]))->value, HKL_EPSILON);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(distance)
{
	HklGeometry *g1 = NULL;
	HklGeometry *g2 = NULL;
	HklHolder *holder = NULL;

	g1 = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g1);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	g2 = hkl_geometry_new_copy(g1);

	hkl_geometry_set_values_v(g1, 3, 0., 0., 0.);
	hkl_geometry_set_values_v(g2, 3, 1., 1., 1.);
	HKL_ASSERT_DOUBLES_EQUAL(3, hkl_geometry_distance(g1, g2), HKL_EPSILON);

	hkl_geometry_free(g1);
	hkl_geometry_free(g2);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_holder );
HKL_TEST( get_axis );
HKL_TEST( update );
HKL_TEST( set_values );
HKL_TEST( distance );

HKL_TEST_SUITE_END
