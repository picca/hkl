#include <math.h>

#include <hkl/hkl-vector.h>
#include <hkl/hkl-matrix.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME vector

HKL_TEST_SUITE_FUNC(init)
{
	HklVector v;

	hkl_vector_init(&v, 1, 2, 3);

	HKL_ASSERT_DOUBLES_EQUAL(1., v.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(2., v.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(3., v.data[2], HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklVector v1 = {{0.0, 1.0, 2.0}};
	HklVector v2 = {{1.0, 2.0, 3.0}};

	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&v1, &v1));
	HKL_ASSERT_EQUAL(1, hkl_vector_cmp(&v1, &v2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(is_opposite)
{
	HklVector v_ref = {{0, 1, 2}};
	HklVector v1 = {{1, 2, 3}};
	HklVector v2 = {{0, -1, -2}};

	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_vector_is_opposite(&v_ref, &v1));
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_vector_is_opposite(&v_ref, &v2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(norm2)
{
	HklVector v1 = {{0.0, 1.0, 2.0}};
	HklVector v2 = {{-1.0, 1.0, 2.0}};

	HKL_ASSERT_DOUBLES_EQUAL(sqrt(5.0), hkl_vector_norm2(&v1), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(sqrt(6.0), hkl_vector_norm2(&v2), HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(normalize)
{
	HklVector v_ref = {{1. /sqrt(2.), 1. / sqrt(2.), 0.}};
	HklVector v = {{1., 1., 0.}};

	hkl_vector_normalize(&v);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&v_ref, &v));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(scalar_product)
{
	HklVector v = {{0.0, 1.0, 2.0}};

	double scalar = hkl_vector_scalar_product(&v, &v);
	HKL_ASSERT_DOUBLES_EQUAL( 5.0, scalar, HKL_EPSILON );

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(vectorial_product)
{
	HklVector v = {{0.0, 1.0, 2.0}};
	HklVector v1 = {{1.0, 2.0, 3.0}};
	HklVector v_ref = {{-1.0, 2.0, -1.0}};

	hkl_vector_vectorial_product(&v, &v1);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&v_ref, &v));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(angle)
{
	double angle;
	HklVector v = {{1., 0., 0.}};
	HklVector v1 = {{1., 1., 0.}};
	HklVector v2 = {{1., 1., .5}};
	HklVector v3 = {{1., .5, -1}};
	HklVector v4 = {{0., 1., 0.}};
	HklVector v5 = {{0., -1., 0.}};

	angle = hkl_vector_angle(&v, &v);
	HKL_ASSERT_DOUBLES_EQUAL(0., angle, HKL_EPSILON);

	angle = hkl_vector_angle(&v, &v1);
	HKL_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, HKL_EPSILON);

	angle = hkl_vector_angle(&v2, &v3);
	HKL_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, HKL_EPSILON);

	angle = hkl_vector_angle(&v, &v4);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, angle, HKL_EPSILON);

	angle = hkl_vector_angle(&v, &v5);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, angle, HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(rotated_around_vector)
{
	HklVector x = {{1, 0, 0}};
	HklVector z = {{0, 0, 1}};
	HklVector y_ref = {{0, 1, 0}};

	hkl_vector_rotated_around_vector(&x, &z, 90*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&y_ref, &x));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(times_smatrix)
{
	HklMatrix m = {{{ 1.0, 3.0,-2.0},
		{10.0, 5.0, 5.0},
		{-3.0, 2.0, 0.0}}
	};
	HklVector v = {{1.0, 2.0, 3.0}};
	HklVector v_ref = {{12., 19., 8.}};

	hkl_vector_times_smatrix(&v, &m);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&v_ref, &v));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( init );
HKL_TEST( cmp );
HKL_TEST( is_opposite );
HKL_TEST( norm2 );
HKL_TEST( normalize );
HKL_TEST( scalar_product );
HKL_TEST( vectorial_product );
HKL_TEST( angle );
HKL_TEST( rotated_around_vector );
HKL_TEST( times_smatrix );

HKL_TEST_SUITE_END
