#include <math.h>

#include <hkl/config.h>
#include <hkl/svector.h>
#include <hkl/smatrix.h>

#include "test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME smatrix

HKL_TEST_SUITE_FUNC(cmp)
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

HKL_TEST_SUITE_FUNC(from_euler)
{
	struct hkl_smatrix m_ref = {{{             1./2.,             -1./2., sqrt(2)/2.},
		{ sqrt(2.)/4.+1./2., -sqrt(2.)/4.+1./2.,     -1./2.},
		{-sqrt(2.)/4.+1./2.,  sqrt(2.)/4.+1./2.,      1./2.}}};
	struct hkl_smatrix m;

	hkl_smatrix_from_euler(&m, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_smatrix_cmp(&m_ref, &m));
}

HKL_TEST_SUITE_FUNC(from_two_svector)
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

HKL_TEST_SUITE_FUNC(times_svector)
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

HKL_TEST_SUITE_FUNC(times_smatrix)
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

HKL_TEST_SUITE_FUNC(transpose)
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

HKL_TEST_SUITE_BEGIN

	HKL_TEST(cmp);
	HKL_TEST(from_euler);
	HKL_TEST(from_two_svector);
	HKL_TEST(times_svector);
	HKL_TEST(times_smatrix);
	HKL_TEST(transpose);

HKL_TEST_SUITE_END
