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
	HklAxes *axes = NULL;
	HklHolder *holder = NULL;

	axes = hkl_axes_new();
	holder = hkl_holder_new(axes);

	// On peut ajouter un Axe dans la partie sample et dans la partie detecteur
	axis = hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	axis = hkl_holder_add_rotation_axis(holder, "b", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);

	hkl_axes_free(axes);
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
*/
HKL_TEST_SUITE_BEGIN

HKL_TEST( add_rotation_axis );

HKL_TEST_SUITE_END
