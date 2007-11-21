#include <math.h>

#include <hkl/config.h>
#include <hkl/quaternion.h>
#include <hkl/axis.h>

#include "test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME axis

HKL_TEST_SUITE_FUNC(add_rotation)
{
	struct hkl_axes axes;
	struct hkl_axis * axis = NULL;
	struct hkl_svector axis_v = {{0, 0, 1}};

	hkl_axes_init(&axes, 0);
	HKL_ASSERT_EQUAL((size_t)0, axes.len);
	HKL_ASSERT_EQUAL((size_t)0, axes.alloc);
	HKL_ASSERT_EQUAL((struct hkl_axis *)NULL, axes.axes);

	axis = hkl_axes_add_rotation(&axes, "omega", &axis_v);
	HKL_ASSERT_EQUAL((size_t)1, axes.len);
	HKL_ASSERT_EQUAL((size_t)24, axes.alloc);
	HKL_ASSERT_EQUAL(axis, &axes.axes[0]);

	axis = hkl_axes_add_rotation(&axes, "tth", &axis_v);
	HKL_ASSERT_EQUAL((size_t)2, axes.len);
	HKL_ASSERT_EQUAL((size_t)24, axes.alloc);
	HKL_ASSERT_EQUAL(axis, &axes.axes[1]);

	hkl_axes_release(&axes);
	HKL_ASSERT_EQUAL((size_t)0, axes.len);
	HKL_ASSERT_EQUAL((size_t)0, axes.alloc);
	HKL_ASSERT_EQUAL((struct hkl_axis *)NULL, axes.axes);
}


HKL_TEST_SUITE_FUNC(get_distance)
{
	struct hkl_axes axes1;
	struct hkl_axes axes2;

	struct hkl_axis *A, *B;

	struct hkl_svector axis_v = {{0, 0, 1}};

	hkl_axes_init(&axes1, 1);
	A = hkl_axes_add_rotation(&axes1, "omega", &axis_v);

	hkl_axes_init(&axes2, 1);
	B = hkl_axes_add_rotation(&axes2, "omega", &axis_v);

	A->config.current = 10 * HKL_DEGTORAD;
	A->config.consign = 10 * HKL_DEGTORAD;
	B->config.current =-10 * HKL_DEGTORAD;
	B->config.consign =-10 * HKL_DEGTORAD;

	// get_distance
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

	hkl_axes_release(&axes1);
	hkl_axes_release(&axes2);
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_rotation );
HKL_TEST( get_distance );

HKL_TEST_SUITE_END
