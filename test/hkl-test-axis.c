#include <math.h>

#include <hkl/hkl-axes.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME axis

HKL_TEST_SUITE_FUNC(add_rotation)
{
	HklAxes *axes = NULL;
	HklAxis *axis = NULL;
	HklAxis *tmp = NULL;
	HklVector v_axis = {{0, 0, 1}};

	axes = hkl_axes_new();
	HKL_ASSERT_EQUAL(0, axes->len);
	HKL_ASSERT_EQUAL(0, axes->alloc);
	HKL_ASSERT_EQUAL((HklAxis *)NULL, axes->axes);

	axis = hkl_axes_add_rotation(axes, "omega", &v_axis);
	HKL_ASSERT_EQUAL((size_t)1, axes->len);
	HKL_ASSERT_EQUAL((size_t)24, axes->alloc);
	HKL_ASSERT_EQUAL(axis, &axes->axes[0]);

	// can not add two times the same axes, must return the same axis
	tmp = axis;
	axis = hkl_axes_add_rotation(axes, "omega", &v_axis);
	HKL_ASSERT_EQUAL(tmp, axis);

	axis = hkl_axes_add_rotation(axes, "tth", &v_axis);
	HKL_ASSERT_EQUAL((size_t)2, axes->len);
	HKL_ASSERT_EQUAL((size_t)24, axes->alloc);
	HKL_ASSERT_EQUAL(axis, &axes->axes[1]);


	hkl_axes_release(axes);
	HKL_ASSERT_EQUAL((size_t)0, axes->len);
	HKL_ASSERT_EQUAL((size_t)0, axes->alloc);
	HKL_ASSERT_EQUAL((HklAxis *)NULL, axes->axes);

	hkl_axes_free(axes);

	return HKL_TEST_PASS;
}


HKL_TEST_SUITE_FUNC(get_distance)
{
	HklAxes *axes1 = NULL;
	HklAxes *axes2 = NULL;

	HklAxis *A, *B;

	HklVector axis_v = {{0, 0, 1}};

	axes1 = hkl_axes_new();
	A = hkl_axes_add_rotation(axes1, "omega", &axis_v);

	axes2 = hkl_axes_new();
	B = hkl_axes_add_rotation(axes2, "omega", &axis_v);

	A->config.current = 10 * HKL_DEGTORAD;
	A->config.consign = 10 * HKL_DEGTORAD;
	B->config.current =-10 * HKL_DEGTORAD;
	B->config.consign =-10 * HKL_DEGTORAD;

	// get_distance
	/*
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 90 * HKL_DEGTORAD;
	B->config.current =-90 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 120 * HKL_DEGTORAD;
	B->config.current =-150 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current =-240 * HKL_DEGTORAD;
	B->config.current = 200 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 200 * HKL_DEGTORAD;
	B->config.current = 240 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = -90 * HKL_DEGTORAD;
	B->config.current =-100 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 90 * HKL_DEGTORAD;
	B->config.consign =-90 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 120 * HKL_DEGTORAD;
	B->config.consign =-150 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign =-240 * HKL_DEGTORAD;
	B->config.consign = 200 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 200 * HKL_DEGTORAD;
	B->config.consign = 240 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = -90 * HKL_DEGTORAD;
	B->config.consign =-100 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);
	*/

	hkl_axes_free(axes1);
	hkl_axes_free(axes2);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_rotation );
HKL_TEST( get_distance );

HKL_TEST_SUITE_END
