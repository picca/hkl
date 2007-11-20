#include <math.h>

#include <hkl/config.h>
#include <hkl/svector.h>
#include <hkl/smatrix.h>

#include "test.h"

HKL_TEST_SUITE_FUNC(svector, cmp)
{
	struct hkl_svector v1 = {{0.0, 1.0, 2.0}};
	struct hkl_svector v2 = {{1.0, 2.0, 3.0}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v1, &v1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_svector_cmp(&v1, &v2));
}

HKL_TEST_SUITE_FUNC(svector, is_opposite)
{
	struct hkl_svector v_ref = {{0, 1, 2}};
	struct hkl_svector v1 = {{1, 2, 3}};
	struct hkl_svector v2 = {{0, -1, -2}};

	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_svector_is_opposite(&v_ref, &v1));
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_is_opposite(&v_ref, &v2));
}

HKL_TEST_SUITE_FUNC(smatrix, cmp)
{
	struct hkl_smatrix m1 = {{{0.0, 1.0, 2.0},
		{3.0, 4.0, 5.0},
		{6.0, 7.0, 8.0}}};

	struct hkl_smatrix m2 = {{{1.0, 1.0, 2.0},
		{3.0, 4.0, 5.0},
		{6.0, 7.0, 8.0}}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m1, &m1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_smatrix_cmp(&m1, &m2));
}

HKL_TEST_SUITE_FUNC(smatrix, from_euler)
{
	struct hkl_smatrix m_ref = {{{             1./2.,             -1./2., sqrt(2)/2.},
		{ sqrt(2.)/4.+1./2., -sqrt(2.)/4.+1./2.,     -1./2.},
		{-sqrt(2.)/4.+1./2.,  sqrt(2.)/4.+1./2.,      1./2.}}};
	struct hkl_smatrix m;

	hkl_smatrix_from_euler(&m, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m_ref, &m));
}

HKL_TEST_SUITE_FUNC(svector, norm2)
{
	struct hkl_svector v1 = {{0.0, 1.0, 2.0}};
	struct hkl_svector v2 = {{-1.0, 1.0, 2.0}};

	HKL_ASSERT_DOUBLES_EQUAL(sqrt(5.0), hkl_svector_norm2(&v1), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(sqrt(6.0), hkl_svector_norm2(&v2), HKL_EPSILON);
}

HKL_TEST_SUITE_FUNC(svector, normalize)
{
	struct hkl_svector v_ref = {{1. /sqrt(2.), 1. / sqrt(2.), 0.}};
	struct hkl_svector v = {{1., 1., 0.}};

	hkl_svector_normalize(&v);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_ref, &v));
}

HKL_TEST_SUITE_FUNC(svector, scalar_product)
{
	struct hkl_svector v = {{0.0, 1.0, 2.0}};

	double scalar = hkl_svector_scalar_product(&v, &v);
	HKL_ASSERT_DOUBLES_EQUAL( 5.0, scalar, HKL_EPSILON );
}

HKL_TEST_SUITE_FUNC(svector, vectorial_product)
{
	struct hkl_svector v = {{0.0, 1.0, 2.0}};
	struct hkl_svector v1 = {{1.0, 2.0, 3.0}};
	struct hkl_svector v_ref = {{-1.0, 2.0, -1.0}};

	hkl_svector_vectorial_product(&v, &v1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_ref, &v));
}

HKL_TEST_SUITE_FUNC(svector, angle)
{
	double angle;
	struct hkl_svector v = {{1., 0., 0.}};
	struct hkl_svector v1 = {{1., 1., 0.}};
	struct hkl_svector v2 = {{1., 1., .5}};
	struct hkl_svector v3 = {{1., .5, -1}};

	angle = hkl_svector_angle(&v, &v);
	HKL_ASSERT_DOUBLES_EQUAL(0., angle, HKL_EPSILON);

	angle = hkl_svector_angle(&v, &v1);
	HKL_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, HKL_EPSILON);

	angle = hkl_svector_angle(&v2, &v3);
	HKL_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, HKL_EPSILON);
}

HKL_TEST_SUITE_FUNC(smatrix, from_two_svector)
{
	struct hkl_svector v1 = {{0.0, 1.0, 2.0}};
	struct hkl_svector v2 = {{1.0, 2.0, 3.0}};
	struct hkl_smatrix m_ref = {{{0.0,             5.0 / sqrt(30.0), -1.0 / sqrt(6.0)},
		{1.0 / sqrt(5.0), 2.0 / sqrt(30.0),  2.0 / sqrt(6.0)},
		{2.0 / sqrt(5.0),-1.0 / sqrt(30.0), -1.0 / sqrt(6.0)}}
	};
	struct hkl_smatrix m;

	hkl_smatrix_from_two_svector(&m, &v1, &v2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m_ref, &m));
}

HKL_TEST_SUITE_FUNC(svector, rotated_around_vector)
{
	struct hkl_svector x = {{1, 0, 0}};
	struct hkl_svector z = {{0, 0, 1}};
	struct hkl_svector y_ref = {{0, 1, 0}};

	hkl_svector_rotated_around_vector(&x, &z, 90*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&y_ref, &x));
}

HKL_TEST_SUITE_FUNC(svector, times_smatrix)
{
	struct hkl_smatrix m = {{{ 1.0, 3.0,-2.0},
		{10.0, 5.0, 5.0},
		{-3.0, 2.0, 0.0}}
	};
	struct hkl_svector v = {{1.0, 2.0, 3.0}};
	struct hkl_svector v_ref = {{12., 19., 8.}};

	hkl_svector_times_smatrix(&v, &m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_ref, &v));
}

HKL_TEST_SUITE_FUNC(smatrix, times_svector)
{
	struct hkl_smatrix m = {{{ 1.0, 3.0,-2.0},
		{10.0, 5.0, 5.0},
		{-3.0, 2.0, 0.0}}
	};
	struct hkl_svector v = {{1, 2, 3}};
	struct hkl_svector v_ref = {{1, 35, 1}};

	hkl_smatrix_times_svector(&m, &v);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_ref, &v));
}

HKL_TEST_SUITE_FUNC(smatrix, times_smatrix)
{
	struct hkl_smatrix m_ref = {{{37., 14., 13.},
		{45., 65.,  5.},
		{17.,  1., 16.}}
	};

	struct hkl_smatrix m = {{{ 1., 3.,-2.},
		{10., 5., 5.},
		{-3., 2., 0.}}
	};

	hkl_smatrix_times_smatrix(&m, &m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m_ref, &m));
}

HKL_TEST_SUITE_FUNC(smatrix, transpose)
{
	struct hkl_smatrix m_ref = {{{37., 14., 13.},
		{45., 65.,  5.},
		{17.,  1., 16.}}
	};

	struct hkl_smatrix m = {{{37., 45., 17.},
		{14., 65.,  1.},
		{13.,  5., 16.}}
	};

	hkl_smatrix_transpose(&m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m_ref, &m));
}

HKL_TEST_SUITE_BEGIN(svector)
	HKL_TEST_SUITE_ADD_FUNC(svector, cmp);
	HKL_TEST_SUITE_ADD_FUNC(svector, is_opposite);
	HKL_TEST_SUITE_ADD_FUNC(smatrix, cmp);
	HKL_TEST_SUITE_ADD_FUNC(smatrix, from_euler);
	HKL_TEST_SUITE_ADD_FUNC(svector, norm2);
	HKL_TEST_SUITE_ADD_FUNC(svector, normalize);
	HKL_TEST_SUITE_ADD_FUNC(svector, scalar_product);
	HKL_TEST_SUITE_ADD_FUNC(svector, vectorial_product);
	HKL_TEST_SUITE_ADD_FUNC(svector, angle);
	HKL_TEST_SUITE_ADD_FUNC(smatrix, from_two_svector);
	HKL_TEST_SUITE_ADD_FUNC(svector, rotated_around_vector);
	HKL_TEST_SUITE_ADD_FUNC(svector, times_smatrix);
	HKL_TEST_SUITE_ADD_FUNC(smatrix, times_svector);
	HKL_TEST_SUITE_ADD_FUNC(smatrix, times_smatrix);
	HKL_TEST_SUITE_ADD_FUNC(smatrix, transpose);
HKL_TEST_SUITE_END
