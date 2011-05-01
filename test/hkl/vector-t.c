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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl.h>
#include <tap/basic.h>

static void init(void)
{
	HklVector v;

	hkl_vector_init(&v, 1, 2, 3);

	is_double(1., v.data[0], HKL_EPSILON, __func__);
	is_double(2., v.data[1], HKL_EPSILON, __func__);
	is_double(3., v.data[2], HKL_EPSILON, __func__);
}

static void cmp(void)
{
	HklVector v1 = {{0.0, 1.0, 2.0}};
	HklVector v2 = {{1.0, 2.0, 3.0}};

	ok(0 == hkl_vector_cmp(&v1, &v1), __func__);
	ok(1 == hkl_vector_cmp(&v1, &v2), __func__);
}

static void is_opposite(void)
{
	HklVector v_ref = {{0, 1, 2}};
	HklVector v1 = {{1, 2, 3}};
	HklVector v2 = {{0, -1, -2}};

	ok(HKL_FALSE == hkl_vector_is_opposite(&v_ref, &v1), __func__);
	ok(HKL_TRUE == hkl_vector_is_opposite(&v_ref, &v2), __func__);
}

static void norm2(void)
{
	HklVector v1 = {{0.0, 1.0, 2.0}};
	HklVector v2 = {{-1.0, 1.0, 2.0}};

	is_double(sqrt(5.0), hkl_vector_norm2(&v1), HKL_EPSILON, __func__);
	is_double(sqrt(6.0), hkl_vector_norm2(&v2), HKL_EPSILON, __func__);
}

static void normalize(void)
{
	HklVector v_ref = {{1. /sqrt(2.), 1. / sqrt(2.), 0.}};
	HklVector v = {{1., 1., 0.}};

	hkl_vector_normalize(&v);
	ok(0 == hkl_vector_cmp(&v_ref, &v), __func__);
}

static void scalar_product(void)
{
	HklVector v = {{0.0, 1.0, 2.0}};

	double scalar = hkl_vector_scalar_product(&v, &v);
	is_double( 5.0, scalar, HKL_EPSILON, __func__ );
}

static void vectorial_product(void)
{
	HklVector v = {{0.0, 1.0, 2.0}};
	HklVector v1 = {{1.0, 2.0, 3.0}};
	HklVector v_ref = {{-1.0, 2.0, -1.0}};

	hkl_vector_vectorial_product(&v, &v1);
	ok(0 == hkl_vector_cmp(&v_ref, &v), __func__);
}

static void angle(void)
{
	double angle;
	HklVector v = {{1., 0., 0.}};
	HklVector v1 = {{1., 1., 0.}};
	HklVector v2 = {{1., 1., .5}};
	HklVector v3 = {{1., .5, -1}};
	HklVector v4 = {{0., 1., 0.}};
	HklVector v5 = {{0., -1., 0.}};

	angle = hkl_vector_angle(&v, &v);
	is_double(0., angle, HKL_EPSILON, __func__);

	angle = hkl_vector_angle(&v, &v1);
	is_double(acos(1./sqrt(2.)), angle, HKL_EPSILON, __func__);

	angle = hkl_vector_angle(&v2, &v3);
	is_double(acos(1./2.25), angle, HKL_EPSILON, __func__);

	angle = hkl_vector_angle(&v, &v4);
	is_double(90 * HKL_DEGTORAD, angle, HKL_EPSILON, __func__);

	angle = hkl_vector_angle(&v, &v5);
	is_double(90 * HKL_DEGTORAD, angle, HKL_EPSILON, __func__);
}

static void oriented_angle(void)
{
	double angle;
	HklVector v = {{1., 0., 0.}};
	HklVector v1 = {{1., 1., 0.}};
	HklVector v2 = {{0., 1., 0.}};
	HklVector v3 = {{0., -1., 0.}};
	HklVector ref = {{0, 0, 1}};

	angle = hkl_vector_oriented_angle(&v, &v, &ref);
	is_double(0., angle, HKL_EPSILON, __func__);

	angle = hkl_vector_oriented_angle(&v, &v1, &ref);
	is_double(acos(1./sqrt(2.)), angle, HKL_EPSILON, __func__);

	angle = hkl_vector_oriented_angle(&v, &v2, &ref);
	is_double(90 * HKL_DEGTORAD, angle, HKL_EPSILON, __func__);

	angle = hkl_vector_oriented_angle(&v, &v3, &ref);
	is_double(-90 * HKL_DEGTORAD, angle, HKL_EPSILON, __func__);
}

static void oriented_angle_points(void)
{
	double angle;
	HklVector v = {{1., 0., 1.}};
	HklVector v1 = {{1., 1., 1.}};
	HklVector v2 = {{0., 1., 1.}};
	HklVector v3 = {{0., -1., 1.}};
	HklVector ref = {{0, 0, 1}};

	angle = hkl_vector_oriented_angle_points(&v, &ref, &v, &ref);
	is_double(0., angle, HKL_EPSILON, __func__);

	angle = hkl_vector_oriented_angle_points(&v, &ref, &v1, &ref);
	is_double(acos(1./sqrt(2.)), angle, HKL_EPSILON, __func__);

	angle = hkl_vector_oriented_angle_points(&v, &ref, &v2, &ref);
	is_double(90 * HKL_DEGTORAD, angle, HKL_EPSILON, __func__);

	angle = hkl_vector_oriented_angle_points(&v, &ref, &v3, &ref);
	is_double(-90 * HKL_DEGTORAD, angle, HKL_EPSILON, __func__);
}

static void rotated_around_vector(void)
{
	HklVector x = {{1, 0, 0}};
	HklVector z = {{0, 0, 1}};
	HklVector y_ref = {{0, 1, 0}};

	hkl_vector_rotated_around_vector(&x, &z, 90*HKL_DEGTORAD);
	ok(0 == hkl_vector_cmp(&y_ref, &x), __func__);
}

static void rotated_around_line(void)
{
	HklVector x = {{1, 0, 0}};
	HklVector c1 = {{0, 0, 0}};
	HklVector c2 = {{0, 0, 1}};
	HklVector x_ref = {{1, 0, 0}};
	HklVector y_ref = {{0, 1, 0}};

	hkl_vector_rotated_around_line(&x, 0*HKL_DEGTORAD, &c1, &c2);
	ok(0 == hkl_vector_cmp(&x_ref, &x), __func__);
	
	hkl_vector_rotated_around_line(&x, 90*HKL_DEGTORAD, &c1, &c2);
	ok(0 == hkl_vector_cmp(&y_ref, &x), __func__);
}

static void times_matrix(void)
{
	HklMatrix m = {{{ 1.0, 3.0,-2.0},
			{10.0, 5.0, 5.0},
			{-3.0, 2.0, 0.0}}
	};
	HklVector v = {{1.0, 2.0, 3.0}};
	HklVector v_ref = {{12., 19., 8.}};

	hkl_vector_times_matrix(&v, &m);
	ok(0 == hkl_vector_cmp(&v_ref, &v), __func__);
}

static void project_on_plan(void)
{
	HklVector v;
	HklVector v_ref = {{1, 0, 0}};
	HklVector v1_ref = {{1, 0, 1}};
	HklVector v2_ref = {{1, 0, -2}};
	HklVector v1 = {{1, 0, 2}};
	HklVector plan = {{0, 0, 1}};
	HklVector point1 = {{0, 0, 1}};
	HklVector point2 = {{0, 0, -2}};

	v = v1;
	hkl_vector_project_on_plan(&v, &plan);
	ok(0 == hkl_vector_cmp(&v_ref, &v), __func__);

	v = v1;
	hkl_vector_project_on_plan_with_point(&v, &plan, &point1);
	ok(0 == hkl_vector_cmp(&v1_ref, &v), __func__);

	v = v1;
	hkl_vector_project_on_plan_with_point(&v, &plan, &point2);
	ok(0 == hkl_vector_cmp(&v2_ref, &v), __func__);
}

int main(int argc, char** argv)
{
	plan(32);

	init();
	cmp();
	is_opposite();
	norm2();
	normalize();
	scalar_product();
	vectorial_product();
	angle();
	oriented_angle();
	oriented_angle_points();
	rotated_around_vector();
	rotated_around_line();
	times_matrix();
	project_on_plan();

	return 0;
}
