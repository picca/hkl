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
	hkl_axes * axes = NULL;
	hkl_axis * axis = NULL;
	static hkl_svector axis_v = {{0, 0, 1}};

	hkl_axes_init(axes, 0);
	CPPUNIT_ASSERT_EQUAL((size_t)0, axes->len);
	CPPUNIT_ASSERT_EQUAL((size_t)0, axes->alloc);
	CPPUNIT_ASSERT_EQUAL((hkl_axis *)NULL, axes->axes);

	axis = ::hkl_axes_add_rotation(axes, "omega", &axis_v);
	CPPUNIT_ASSERT_EQUAL((size_t)1, axes->len);
	CPPUNIT_ASSERT_EQUAL((size_t)1, axes->alloc);
	CPPUNIT_ASSERT_EQUAL(axis, axes->axes);

	axis = ::hkl_axes_add_rotation(axes, "omega", &axis_v);
	CPPUNIT_ASSERT_EQUAL((hkl_axis *)NULL, axis);
	hkl_axes_release(axes);
}


void
AxisTest::hkl_axes_get_distance(void)
{
	hkl_axes * axes1 = NULL;
	hkl_axes * axes2 = NULL;

	hkl_svector axe = {{0, 0, 1}};
	hkl::axe::Rotation A("toto", "titi", -2*M_PI, 10 * HKL_DEGTORAD, 2*M_PI, &axe);
	hkl::axe::Rotation B("toto", "titi", -2*M_PI, -10 * HKL_DEGTORAD, 2*M_PI, &axe);

	// get_distance
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);

	A.set_current(90 * HKL_DEGTORAD);
	B.set_current(-90 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_current(120 * HKL_DEGTORAD);
	B.set_current(-150 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_current(-240 * HKL_DEGTORAD);
	B.set_current(200 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_current(200 * HKL_DEGTORAD);
	B.set_current(240 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_current(-90 * HKL_DEGTORAD);
	B.set_current(-100 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	// get_distance_consign
	CPPUNIT_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_consign(90 * HKL_DEGTORAD);
	B.set_consign(-90 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_consign(120 * HKL_DEGTORAD);
	B.set_consign(-150 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_consign(-240 * HKL_DEGTORAD);
	B.set_consign(200 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_consign(200 * HKL_DEGTORAD);
	B.set_consign(240 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);

	A.set_consign(-90 * HKL_DEGTORAD);
	B.set_consign(-100 * HKL_DEGTORAD);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance(B), HKL_EPSILON);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, A.get_distance_consign(B), HKL_EPSILON);
}
