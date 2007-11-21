#include <math.h>

#include <hkl/config.h>
#include <hkl/svector.h>
#include <hkl/smatrix.h>

#include "test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME svector

HKL_TEST_SUITE_FUNC(cmp)
{
	struct hkl_svector v1 = {{0.0, 1.0, 2.0}};
	struct hkl_svector v2 = {{1.0, 2.0, 3.0}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v1, &v1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_svector_cmp(&v1, &v2));
}

HKL_TEST_SUITE_FUNC(is_opposite)
{
	struct hkl_svector v_ref = {{0, 1, 2}};
	struct hkl_svector v1 = {{1, 2, 3}};
	struct hkl_svector v2 = {{0, -1, -2}};

	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_svector_is_opposite(&v_ref, &v1));
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_is_opposite(&v_ref, &v2));
}

HKL_TEST_SUITE_FUNC(norm2)
{
	struct hkl_svector v1 = {{0.0, 1.0, 2.0}};
	struct hkl_svector v2 = {{-1.0, 1.0, 2.0}};

	HKL_ASSERT_DOUBLES_EQUAL(sqrt(5.0), hkl_svector_norm2(&v1), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(sqrt(6.0), hkl_svector_norm2(&v2), HKL_EPSILON);
}

HKL_TEST_SUITE_FUNC(normalize)
{
	struct hkl_svector v_ref = {{1. /sqrt(2.), 1. / sqrt(2.), 0.}};
	struct hkl_svector v = {{1., 1., 0.}};

	hkl_svector_normalize(&v);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_ref, &v));
}

HKL_TEST_SUITE_FUNC(scalar_product)
{
	struct hkl_svector v = {{0.0, 1.0, 2.0}};

	double scalar = hkl_svector_scalar_product(&v, &v);
	HKL_ASSERT_DOUBLES_EQUAL( 5.0, scalar, HKL_EPSILON );
}

HKL_TEST_SUITE_FUNC(vectorial_product)
{
	struct hkl_svector v = {{0.0, 1.0, 2.0}};
	struct hkl_svector v1 = {{1.0, 2.0, 3.0}};
	struct hkl_svector v_ref = {{-1.0, 2.0, -1.0}};

	hkl_svector_vectorial_product(&v, &v1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&v_ref, &v));
}

HKL_TEST_SUITE_FUNC(angle)
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

HKL_TEST_SUITE_FUNC(rotated_around_vector)
{
	struct hkl_svector x = {{1, 0, 0}};
	struct hkl_svector z = {{0, 0, 1}};
	struct hkl_svector y_ref = {{0, 1, 0}};

	hkl_svector_rotated_around_vector(&x, &z, 90*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&y_ref, &x));
}

HKL_TEST_SUITE_FUNC(times_smatrix)
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

HKL_TEST_SUITE_BEGIN

HKL_TEST(cmp);
HKL_TEST(is_opposite);
HKL_TEST(norm2);
HKL_TEST(normalize);
HKL_TEST(scalar_product);
HKL_TEST(vectorial_product);
HKL_TEST(angle);
HKL_TEST(rotated_around_vector);
HKL_TEST(times_smatrix);

HKL_TEST_SUITE_END
