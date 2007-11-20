#include <math.h>

#include "hkl/config.h"
#include "hkl/svector.h"
#include "hkl/smatrix.h"
#include "hkl/quaternion.h"

#include "test.h"

HKL_TEST_SUITE_FUNC(quaternion, cmp)
{
	struct hkl_quaternion q_ref = {{1., 2., 3., 4.}};
	struct hkl_quaternion q = {{1., 2., 3., 4.}};
	struct hkl_quaternion q1 = {{1., 1., 3., 4.}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_quaternion_cmp(&q_ref, &q1));

	// test the assignation
	q1 = q_ref;
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q1));
}

HKL_TEST_SUITE_FUNC(quaternion, from_svector)
{
	struct hkl_quaternion q_ref = {{0, 1, -1, .5}};
	struct hkl_svector v = {{1., -1., .5}};
	struct hkl_quaternion q;

	hkl_quaternion_from_svector(&q, &v);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));
}

HKL_TEST_SUITE_FUNC(quaternion, from_angle_and_axe)
{
	struct hkl_quaternion q_ref1 = {{1, 0, 0, 0}};
	struct hkl_quaternion q_ref2 = {{sqrt(2.)/2., sqrt(2./9.), -sqrt(2./9.), sqrt(1./18.)}};
	struct hkl_svector v_ref2 = {{1., -1., .5}};
	struct hkl_quaternion q;

	hkl_quaternion_from_angle_and_axe(&q, 0, &v_ref2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref1, &q));

	hkl_quaternion_from_angle_and_axe(&q, 90. * HKL_DEGTORAD, &v_ref2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref2, &q));
}

HKL_TEST_SUITE_FUNC(quaternion, times_quaternion)
{
	struct hkl_quaternion q_ref = {{-28., 4., 6., 8.}};
	struct hkl_quaternion q = {{1., 2., 3., 4.}};

	hkl_quaternion_times_quaternion(&q, &q);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));
}

HKL_TEST_SUITE_FUNC(quaternion, norm2)
{
	struct hkl_quaternion q = {{1., 2., 3., 4.}};

	HKL_ASSERT_DOUBLES_EQUAL(sqrt(30.), hkl_quaternion_norm2(&q), HKL_EPSILON);
}

HKL_TEST_SUITE_FUNC(quaternion, conjugate)
{
	struct hkl_quaternion q_ref = {{1., -2., -3., -4.}};
	struct hkl_quaternion q = {{1., 2., 3., 4.}};

	hkl_quaternion_conjugate(&q);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));
}

HKL_TEST_SUITE_FUNC(quaternion, to_smatrix)
{
	struct hkl_quaternion q_ref = {{1./sqrt(2), 0, 0, 1./sqrt(2)}};
	struct hkl_smatrix m_ref = {{{0,-1, 0},
		{1, 0, 0},
		{0, 0, 1}}};
	struct hkl_smatrix m;

	hkl_quaternion_to_smatrix(&q_ref, &m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m_ref, &m));
}

HKL_TEST_SUITE_FUNC(quaternion, to_angle_and_axe)
{
	struct hkl_svector v_ref = {{0 ,0, 1}};
	struct hkl_svector v_null = {{0 ,0, 0}};
	struct hkl_quaternion q_I = {{1, 0, 0, 0}};

	int i;
	double angle_ref;
	double angle;
	struct hkl_svector v;
	struct hkl_quaternion q;


	// test the q = (1, 0, 0, 0) solution axe == (0, 0, 0) and angle = 0.
	hkl_quaternion_to_angle_and_axe(&q_I, &angle, &v);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_null, &v));
	HKL_ASSERT_EQUAL(0., angle);

	// test other cases
	for(i=-180; i<180; i++) {
		angle_ref = i *  HKL_DEGTORAD;
		hkl_quaternion_from_angle_and_axe(&q, angle_ref, &v_ref);
		hkl_quaternion_to_angle_and_axe(&q, &angle, &v);

		if (hkl_svector_cmp(&v_ref, &v))
			HKL_ASSERT_DOUBLES_EQUAL(angle_ref, angle, HKL_EPSILON);
		else if (hkl_svector_is_opposite(&v, &v_ref))
			HKL_ASSERT_DOUBLES_EQUAL(angle_ref, -angle, HKL_EPSILON);
	}
}

HKL_TEST_SUITE_BEGIN( quaternion )

	HKL_TEST( quaternion, cmp );
	HKL_TEST( quaternion, from_svector );
	HKL_TEST( quaternion, from_angle_and_axe );
	HKL_TEST( quaternion, times_quaternion );
	HKL_TEST( quaternion, norm2 );
	HKL_TEST( quaternion, conjugate );
	HKL_TEST( quaternion, to_smatrix );
	HKL_TEST( quaternion, to_angle_and_axe );

HKL_TEST_SUITE_END;

