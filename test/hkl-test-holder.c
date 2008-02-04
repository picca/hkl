#include <math.h>

#include <hkl/hkl-holder.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME holder

HKL_TEST_SUITE_FUNC(add_rotation_axis)
{
	HklAxis *axis = NULL;
	HklList *axes = NULL;
	HklHolder *holder = NULL;
	unsigned int i;

	axes = hkl_list_new();
	holder = hkl_holder_new(axes);

	// add two different axis
	axis = hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	HKL_ASSERT_EQUAL(1, hkl_holder_size(holder));
	axis = hkl_holder_add_rotation_axis(holder, "b", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	HKL_ASSERT_EQUAL(2, hkl_holder_size(holder));

	// can not add two times the same axes, must return the same axis
	axis = hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, axis);
	HKL_ASSERT_EQUAL(2, hkl_holder_size(holder));

	// release the axes memory as holder do not manage it.
	for(i=0; i<axes->len; ++i)
		hkl_axis_free(axes->list[i]);
	hkl_list_free(axes);

	hkl_holder_free(holder);

	return HKL_TEST_PASS;
}

/*
	void
HolderTest::apply(void)
{
	hkl_svector axe =  {{0, -1, 0}};
	hkl_svector axe1 = {{0, 0, 1}};
	hkl_svector axe2 = {{1, 0, 0}};

	_holder->add_rotation("omega", &axe);
	_holder->add_rotation("gamma", &axe1);

	// Verification of the apply method of the Axe class.
	hkl_quaternion q;
	hkl_quaternion q_ref;

	::hkl_quaternion_from_angle_and_axe(&q, 10 * HKL_DEGTORAD, &axe2);
	::hkl_quaternion_from_angle_and_axe(&q_ref, 10 * HKL_DEGTORAD, &axe2);
	_holder->apply(&q);
	CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
	_holder->apply_consign(&q);
	CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

HKL_TEST_SUITE_FUNC(get_distance)
{
	HklAxes *axes1 = NULL;
	HklAxes *axes2 = NULL;
	unsigned int i;

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

	for(i=0; i<axes1->axes->len; ++i)
		hkl_axis_free(axes1->axes->list[i]);
	for(i=0; i<axes2->axes->len; ++i)
		hkl_axis_free(axes2->axes->list[i]);

	hkl_axes_free(axes1);
	hkl_axes_free(axes2);

	return HKL_TEST_PASS;
}
*/

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_rotation_axis );

HKL_TEST_SUITE_END
