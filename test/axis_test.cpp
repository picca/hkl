#include <cmath>

#include "config.h"
#include "quaternion.h"
#include "axis_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( AxisTest );

void
AxisTest::setUp(void) {}

void
AxisTest::tearDown(void) {}

void
AxisTest::hkl_axes_add_rotation(void)
{
	hkl_axes axes;
	hkl_axis * axis = NULL;
	static hkl_svector axis_v = {{0, 0, 1}};

	hkl_axes_init(&axes, 0);
	CPPUNIT_ASSERT_EQUAL((size_t)0, axes.len);
	CPPUNIT_ASSERT_EQUAL((size_t)0, axes.alloc);
	CPPUNIT_ASSERT_EQUAL((hkl_axis *)NULL, axes.axes);

	axis = ::hkl_axes_add_rotation(&axes, "omega", &axis_v);
	CPPUNIT_ASSERT_EQUAL((size_t)1, axes.len);
	CPPUNIT_ASSERT_EQUAL((size_t)24, axes.alloc);
	CPPUNIT_ASSERT_EQUAL(axis, &axes.axes[0]);

	axis = ::hkl_axes_add_rotation(&axes, "tth", &axis_v);
	CPPUNIT_ASSERT_EQUAL((size_t)2, axes.len);
	CPPUNIT_ASSERT_EQUAL((size_t)24, axes.alloc);
	CPPUNIT_ASSERT_EQUAL(axis, &axes.axes[1]);

	hkl_axes_release(&axes);
	CPPUNIT_ASSERT_EQUAL((size_t)0, axes.len);
	CPPUNIT_ASSERT_EQUAL((size_t)0, axes.alloc);
	CPPUNIT_ASSERT_EQUAL((hkl_axis *)NULL, axes.axes);
}


void
AxisTest::hkl_axes_get_distance(void)
{
	hkl_axes axes1;
	hkl_axes axes2;

	hkl_axis *A, *B;

	static hkl_svector axis_v = {{0, 0, 1}};
	
	hkl_axes_init(&axes1, 1);
	A = ::hkl_axes_add_rotation(&axes1, "omega", &axis_v);

	hkl_axes_init(&axes2, 1);
	B = ::hkl_axes_add_rotation(&axes2, "omega", &axis_v);

	A->config.current = 10 * HKL_DEGTORAD;
	A->config.consign = 10 * HKL_DEGTORAD;
	B->config.current =-10 * HKL_DEGTORAD;
	B->config.consign =-10 * HKL_DEGTORAD;

	// get_distance
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 90 * HKL_DEGTORAD;
	B->config.current =-90 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 120 * HKL_DEGTORAD;
	B->config.current =-150 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current =-240 * HKL_DEGTORAD;
	B->config.current = 200 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 200 * HKL_DEGTORAD;
	B->config.current = 240 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = -90 * HKL_DEGTORAD;
	B->config.current =-100 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 90 * HKL_DEGTORAD;
	B->config.consign =-90 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 120 * HKL_DEGTORAD;
	B->config.consign =-150 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign =-240 * HKL_DEGTORAD;
	B->config.consign = 200 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 200 * HKL_DEGTORAD;
	B->config.consign = 240 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = -90 * HKL_DEGTORAD;
	B->config.consign =-100 * HKL_DEGTORAD;
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, ::hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	hkl_axes_release(&axes1);
	hkl_axes_release(&axes2);
}
