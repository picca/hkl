#include <math.h>

#include <hkl/hkl-vector.h>
#include <hkl/hkl-matrix.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME smatrix

HKL_TEST_SUITE_FUNC(new)
{
	HklMatrix *m;

	m = hkl_matrix_new(1, 0, 0, 0, 1, 0, 0, 0, 1);
	HKL_ASSERT_EQUAL(1, m->data[0][0]);
	HKL_ASSERT_EQUAL(0, m->data[0][1]);
	HKL_ASSERT_EQUAL(0, m->data[0][2]);
	HKL_ASSERT_EQUAL(0, m->data[1][0]);
	HKL_ASSERT_EQUAL(1, m->data[1][1]);
	HKL_ASSERT_EQUAL(0, m->data[1][2]);
	HKL_ASSERT_EQUAL(0, m->data[2][0]);
	HKL_ASSERT_EQUAL(0, m->data[2][1]);
	HKL_ASSERT_EQUAL(1, m->data[2][2]);
	hkl_matrix_free(m);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklMatrix *m;

	m = hkl_matrix_new(1, 0, 0, 0, 1, 0, 0, 0, 1);
	hkl_matrix_set(m, 1, 1, 0, 0, 1, 0, 0, 0, 1);
	HKL_ASSERT_EQUAL(1, m->data[0][0]);
	HKL_ASSERT_EQUAL(1, m->data[0][1]);
	HKL_ASSERT_EQUAL(0, m->data[0][2]);
	HKL_ASSERT_EQUAL(0, m->data[1][0]);
	HKL_ASSERT_EQUAL(1, m->data[1][1]);
	HKL_ASSERT_EQUAL(0, m->data[1][2]);
	HKL_ASSERT_EQUAL(0, m->data[2][0]);
	HKL_ASSERT_EQUAL(0, m->data[2][1]);
	HKL_ASSERT_EQUAL(1, m->data[2][2]);
	hkl_matrix_free(m);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklMatrix m1 = {{{0.0, 1.0, 2.0},
		{3.0, 4.0, 5.0},
		{6.0, 7.0, 8.0}}};

	HklMatrix m2 = {{{1.0, 1.0, 2.0},
		{3.0, 4.0, 5.0},
		{6.0, 7.0, 8.0}}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m1, &m1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_matrix_cmp(&m1, &m2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklMatrix m1 = {{{0.0, 1.0, 2.0},
		{3.0, 4.0, 5.0},
		{6.0, 7.0, 8.0}}};
	HklMatrix *m;

	m = hkl_matrix_new_copy(&m1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m1, m));
	hkl_matrix_free(m);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(from_euler)
{
	HklMatrix m_ref = {{{             1./2.,             -1./2., sqrt(2)/2.},
		{ sqrt(2.)/4.+1./2., -sqrt(2.)/4.+1./2.,     -1./2.},
		{-sqrt(2.)/4.+1./2.,  sqrt(2.)/4.+1./2.,      1./2.}}};
	HklMatrix m;

	hkl_matrix_from_euler(&m, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &m));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(from_two_vector)
{
	HklVector v1 = {{0.0, 1.0, 2.0}};
	HklVector v2 = {{1.0, 2.0, 3.0}};
	HklMatrix m_ref = {{{0.0,             5.0 / sqrt(30.0), -1.0 / sqrt(6.0)},
		{1.0 / sqrt(5.0), 2.0 / sqrt(30.0),  2.0 / sqrt(6.0)},
		{2.0 / sqrt(5.0),-1.0 / sqrt(30.0), -1.0 / sqrt(6.0)}}
	};
	HklMatrix m;

	hkl_matrix_from_two_vector(&m, &v1, &v2);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &m));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(times_vector)
{
	HklMatrix m = {{{ 1.0, 3.0,-2.0},
		{10.0, 5.0, 5.0},
		{-3.0, 2.0, 0.0}}
	};
	HklVector v = {{1, 2, 3}};
	HklVector v_ref = {{1, 35, 1}};

	hkl_matrix_times_vector(&m, &v);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&v_ref, &v));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(times_smatrix)
{
	HklMatrix m_ref = {{{37., 14., 13.},
		{45., 65.,  5.},
		{17.,  1., 16.}}
	};

	HklMatrix m = {{{ 1., 3.,-2.},
		{10., 5., 5.},
		{-3., 2., 0.}}
	};

	hkl_matrix_times_smatrix(&m, &m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &m));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(transpose)
{
	HklMatrix m_ref = {{{37., 14., 13.},
		{45., 65.,  5.},
		{17.,  1., 16.}}
	};

	HklMatrix m = {{{37., 45., 17.},
		{14., 65.,  1.},
		{13.,  5., 16.}}
	};

	hkl_matrix_transpose(&m);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &m));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

	HKL_TEST(new);
	HKL_TEST(set);
	HKL_TEST(cmp);
	HKL_TEST(new_copy);
	HKL_TEST(from_euler);
	HKL_TEST(from_two_vector);
	HKL_TEST(times_vector);
	HKL_TEST(times_smatrix);
	HKL_TEST(transpose);

HKL_TEST_SUITE_END
