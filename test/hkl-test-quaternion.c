/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME quaternion

HKL_TEST_SUITE_FUNC(assignment)
{
	HklQuaternion q = {{1, 0, 0, 0}};
	HklQuaternion copy = q;

	HKL_ASSERT_DOUBLES_EQUAL(1., copy.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., copy.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., copy.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., copy.data[3], HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklQuaternion q_ref = {{1., 2., 3., 4.}};
	HklQuaternion q = {{1., 2., 3., 4.}};
	HklQuaternion q1 = {{1., 1., 3., 4.}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_quaternion_cmp(&q_ref, &q1));

	// test the assignation
	q1 = q_ref;
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q1));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(from_vector)
{
	HklQuaternion q_ref = {{0, 1, -1, .5}};
	HklVector v = {{1., -1., .5}};
	HklQuaternion q;

	hkl_quaternion_from_vector(&q, &v);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(from_angle_and_axe)
{
	HklQuaternion q_ref1 = {{1, 0, 0, 0}};
	HklQuaternion q_ref2 = {{sqrt(2.)/2., sqrt(2./9.), -sqrt(2./9.), sqrt(1./18.)}};
	HklVector v_ref2 = {{1., -1., .5}};
	HklQuaternion q;

	hkl_quaternion_from_angle_and_axe(&q, 0, &v_ref2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref1, &q));

	hkl_quaternion_from_angle_and_axe(&q, 90. * HKL_DEGTORAD, &v_ref2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref2, &q));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(times_quaternion)
{
	HklQuaternion q_ref = {{-28., 4., 6., 8.}};
	HklQuaternion q = {{1., 2., 3., 4.}};

	hkl_quaternion_times_quaternion(&q, &q);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(norm2)
{
	HklQuaternion q = {{1., 2., 3., 4.}};

	HKL_ASSERT_DOUBLES_EQUAL(sqrt(30.), hkl_quaternion_norm2(&q), HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(conjugate)
{
	HklQuaternion q_ref = {{1., -2., -3., -4.}};
	HklQuaternion q = {{1., 2., 3., 4.}};

	hkl_quaternion_conjugate(&q);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_quaternion_cmp(&q_ref, &q));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(to_smatrix)
{
	HklQuaternion q_ref = {{1./sqrt(2), 0, 0, 1./sqrt(2)}};
	HklMatrix m_ref = {{{0,-1, 0},
		{1, 0, 0},
		{0, 0, 1}}};
	HklMatrix m;

	hkl_quaternion_to_smatrix(&q_ref, &m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &m));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(to_angle_and_axe)
{
	HklVector v_ref = {{0 ,0, 1}};
	HklVector v_null = {{0 ,0, 0}};
	HklQuaternion q_I = {{1, 0, 0, 0}};

	int i;
	double angle_ref;
	double angle;
	HklVector v;
	HklQuaternion q;


	// test the q = (1, 0, 0, 0) solution axe == (0, 0, 0) and angle = 0.
	hkl_quaternion_to_angle_and_axe(&q_I, &angle, &v);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&v_null, &v));
	HKL_ASSERT_DOUBLES_EQUAL(0., angle, HKL_EPSILON);

	// test other cases
	for(i=-180; i<180; i++) {
		angle_ref = i *  HKL_DEGTORAD;
		hkl_quaternion_from_angle_and_axe(&q, angle_ref, &v_ref);
		hkl_quaternion_to_angle_and_axe(&q, &angle, &v);

		if (!hkl_vector_cmp(&v_ref, &v))
			HKL_ASSERT_DOUBLES_EQUAL(angle_ref, angle, HKL_EPSILON);
		else if (hkl_vector_is_opposite(&v, &v_ref))
			HKL_ASSERT_DOUBLES_EQUAL(angle_ref, -angle, HKL_EPSILON);
	}

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

	HKL_TEST( assignment );
	HKL_TEST( cmp );
	HKL_TEST( from_vector );
	HKL_TEST( from_angle_and_axe );
	HKL_TEST( times_quaternion );
	HKL_TEST( norm2 );
	HKL_TEST( conjugate );
	HKL_TEST( to_smatrix );
	HKL_TEST( to_angle_and_axe );

HKL_TEST_SUITE_END
