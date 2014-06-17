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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>

#include "hkl-quaternion-private.h"

static void assignment(void)
{
	HklQuaternion q = {{1, 0, 0, 0}};
	HklQuaternion copy = q;

	is_double(1., copy.data[0], HKL_EPSILON, __func__);
	is_double(0., copy.data[1], HKL_EPSILON, __func__);
	is_double(0., copy.data[2], HKL_EPSILON, __func__);
	is_double(0., copy.data[3], HKL_EPSILON, __func__);
}

static void cmp(void)
{
	HklQuaternion q_ref = {{1., 2., 3., 4.}};
	HklQuaternion q = {{1., 2., 3., 4.}};
	HklQuaternion q1 = {{1., 1., 3., 4.}};

	ok(TRUE == hkl_quaternion_cmp(&q_ref, &q), __func__);
	ok(FALSE == hkl_quaternion_cmp(&q_ref, &q1), __func__);

	/* test the assignation */
	q1 = q_ref;
	ok(TRUE == hkl_quaternion_cmp(&q_ref, &q1), __func__);
}

static void init_from_vector(void)
{
	HklQuaternion q_ref = {{0, 1, -1, .5}};
	HklVector v = {{1., -1., .5}};
	HklQuaternion q;

	hkl_quaternion_init_from_vector(&q, &v);
	ok(TRUE == hkl_quaternion_cmp(&q_ref, &q), __func__);
}

static void init_from_angle_and_axe(void)
{
	HklQuaternion q_ref1 = {{1, 0, 0, 0}};
	HklQuaternion q_ref2 = {{sqrt(2.)/2., sqrt(2./9.), -sqrt(2./9.), sqrt(1./18.)}};
	HklVector v_ref2 = {{1., -1., .5}};
	HklQuaternion q;

	hkl_quaternion_init_from_angle_and_axe(&q, 0, &v_ref2);
	ok(TRUE == hkl_quaternion_cmp(&q_ref1, &q), __func__);

	hkl_quaternion_init_from_angle_and_axe(&q, 90. * HKL_DEGTORAD, &v_ref2);
	ok(TRUE == hkl_quaternion_cmp(&q_ref2, &q), __func__);
}

static void times_quaternion(void)
{
	HklQuaternion q_ref = {{-28., 4., 6., 8.}};
	HklQuaternion q = {{1., 2., 3., 4.}};

	hkl_quaternion_times_quaternion(&q, &q);
	ok(TRUE == hkl_quaternion_cmp(&q_ref, &q), __func__);
}

static void norm2(void)
{
	HklQuaternion q = {{1., 2., 3., 4.}};

	is_double(sqrt(30.), hkl_quaternion_norm2(&q), HKL_EPSILON, __func__);
}

static void conjugate(void)
{
	HklQuaternion q_ref = {{1., -2., -3., -4.}};
	HklQuaternion q = {{1., 2., 3., 4.}};

	hkl_quaternion_conjugate(&q);
	ok(TRUE == hkl_quaternion_cmp(&q_ref, &q), __func__);
}

static void to_matrix(void)
{
	HklQuaternion q_ref = {{1./sqrt(2), 0, 0, 1./sqrt(2)}};
	HklMatrix *m_ref = hkl_matrix_new_full(0,-1, 0,
					       1, 0, 0,
					       0, 0, 1);
	HklMatrix *m = hkl_matrix_new();

	hkl_quaternion_to_matrix(&q_ref, m);
	ok(TRUE == hkl_matrix_cmp(m_ref, m), __func__);

	hkl_matrix_free(m_ref);
	hkl_matrix_free(m);
}

static void to_angle_and_axe(void)
{
	HklVector v_ref = {{0 ,0, 1}};
	HklVector v_null = {{0 ,0, 0}};
	HklQuaternion q_I = {{1, 0, 0, 0}};

	int i;
	double angle_ref;
	double angle;
	HklVector v;
	HklQuaternion q;


	/* test the q = (1, 0, 0, 0) solution axe == (0, 0, 0) and angle = 0. */
	hkl_quaternion_to_angle_and_axe(&q_I, &angle, &v);
	ok(0 == hkl_vector_cmp(&v_null, &v), __func__);
	is_double(0., angle, HKL_EPSILON, __func__);

	/* test other cases */
	for(i=-180; i<180; i++) {
		angle_ref = i *  HKL_DEGTORAD;
		hkl_quaternion_init_from_angle_and_axe(&q, angle_ref, &v_ref);
		hkl_quaternion_to_angle_and_axe(&q, &angle, &v);

		if (!hkl_vector_cmp(&v_ref, &v))
			is_double(angle_ref, angle, HKL_EPSILON, __func__);
		else if (hkl_vector_is_opposite(&v, &v_ref))
			is_double(angle_ref, -angle, HKL_EPSILON, __func__);
	}
}

int main(int argc, char** argv)
{
	plan(375);

	assignment();
	cmp();
	init_from_vector();
	init_from_angle_and_axe();
	times_quaternion();
	norm2();
	conjugate();
	to_matrix();
	to_angle_and_axe();

	return 0;
}
