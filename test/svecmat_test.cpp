// File to test matrix and vector implementation.
#include "svecmat_test.h"
#include <assert.h>

CPPUNIT_TEST_SUITE_REGISTRATION( vectorMatrixTest );

void
vectorMatrixTest::setUp(void) {}

void
vectorMatrixTest::tearDown(void) {}

void
vectorMatrixTest::hkl_svector_cmp(void)
{
  hkl_svector v1 = {{0.0, 1.0, 2.0}};
  hkl_svector v2 = {{1.0, 2.0, 3.0}};

  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v1, &v1));
  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_svector_cmp(&v1, &v2));
}

void
vectorMatrixTest::hkl_svector_is_opposite(void)
{
  static hkl_svector v_ref = {{0, 1, 2}};
  static hkl_svector v1 = {{1, 2, 3}};
  static hkl_svector v2 = {{0, -1, -2}};

  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_svector_is_opposite(&v_ref, &v1));
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_is_opposite(&v_ref, &v2));
}

void
vectorMatrixTest::hkl_smatrix_cmp(void)
{
  hkl_smatrix m1 = {{{0.0, 1.0, 2.0},
      {3.0, 4.0, 5.0},
      {6.0, 7.0, 8.0}}
  };

  hkl_smatrix m2 = {{{1.0, 1.0, 2.0},
      {3.0, 4.0, 5.0},
      {6.0, 7.0, 8.0}}
  };

  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m1, &m1));
  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_smatrix_cmp(&m1, &m2));
}

void
vectorMatrixTest::hkl_smatrix_from_euler(void)
{
  hkl_smatrix m_ref = {{{             1./2.,             -1./2., sqrt(2)/2.},
      { sqrt(2.)/4.+1./2., -sqrt(2.)/4.+1./2.,     -1./2.},
      {-sqrt(2.)/4.+1./2.,  sqrt(2.)/4.+1./2.,      1./2.}}
  };
  hkl_smatrix m;
  ::hkl_smatrix_from_euler(&m, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD, 45.*HKL_DEGTORAD);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
vectorMatrixTest::hkl_svector_norm2(void)
{
  hkl_svector v1 = {{0.0, 1.0, 2.0}};
  hkl_svector v2 = {{-1.0, 1.0, 2.0}};

  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(5.0), ::hkl_svector_norm2(&v1), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(sqrt(6.0), ::hkl_svector_norm2(&v2), HKL_EPSILON);
}

void
vectorMatrixTest::hkl_svector_normalize(void)
{
  hkl_svector v_ref = {{1. /sqrt(2.), 1. / sqrt(2.), 0.}};
  hkl_svector v = {{1., 1., 0.}};

  ::hkl_svector_normalize(&v);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v_ref, &v));
}

void
vectorMatrixTest::hkl_svector_scalar_product(void)
{
  hkl_svector v = {{0.0, 1.0, 2.0}};

  double scalar = ::hkl_svector_scalar_product(&v, &v);
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 5.0, scalar, HKL_EPSILON );
}

void
vectorMatrixTest::hkl_svector_vectorial_product(void)
{
  hkl_svector v = {{0.0, 1.0, 2.0}};
  hkl_svector v1 = {{1.0, 2.0, 3.0}};
  hkl_svector v_ref = {{-1.0, 2.0, -1.0}};

  ::hkl_svector_vectorial_product(&v, &v1);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v_ref, &v));
}

void
vectorMatrixTest::hkl_svector_angle(void)
{
  double angle;
  hkl_svector v = {{1., 0., 0.}};
  hkl_svector v1 = {{1., 1., 0.}};
  hkl_svector v2 = {{1., 1., .5}};
  hkl_svector v3 = {{1., .5, -1}};

  angle = ::hkl_svector_angle(&v, &v);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, HKL_EPSILON);

  angle = ::hkl_svector_angle(&v, &v1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, HKL_EPSILON);

  angle = ::hkl_svector_angle(&v2, &v3);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, HKL_EPSILON);
}

void
vectorMatrixTest::hkl_smatrix_from_two_svector(void)
{
  hkl_svector v1 = {{0.0, 1.0, 2.0}};
  hkl_svector v2 = {{1.0, 2.0, 3.0}};
  hkl_smatrix m_ref = {{{0.0,             5.0 / sqrt(30.0), -1.0 / sqrt(6.0)},
      {1.0 / sqrt(5.0), 2.0 / sqrt(30.0),  2.0 / sqrt(6.0)},
      {2.0 / sqrt(5.0),-1.0 / sqrt(30.0), -1.0 / sqrt(6.0)}}
  };
  hkl_smatrix m;

  ::hkl_smatrix_from_two_svector(&m, &v1, &v2);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
vectorMatrixTest::hkl_svector_rotated_around_vector(void)
{
  hkl_svector x = {{1, 0, 0}};
  hkl_svector z = {{0, 0, 1}};
  hkl_svector y_ref = {{0, 1, 0}};

  ::hkl_svector_rotated_around_vector(&x, &z, 90*HKL_DEGTORAD);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&y_ref, &x));
}

void
vectorMatrixTest::hkl_svector_times_smatrix(void)
{
  hkl_smatrix m = {{{ 1.0, 3.0,-2.0},
      {10.0, 5.0, 5.0},
      {-3.0, 2.0, 0.0}}
  };
  hkl_svector v = {{1.0, 2.0, 3.0}};
  hkl_svector v_ref = {{12., 19., 8.}};

  ::hkl_svector_times_smatrix(&v, &m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v_ref, &v));
}

void
vectorMatrixTest::hkl_smatrix_times_svector(void)
{
  hkl_smatrix m = {{{ 1.0, 3.0,-2.0},
      {10.0, 5.0, 5.0},
      {-3.0, 2.0, 0.0}}
  };
  hkl_svector v = {{1, 2, 3}};
  hkl_svector v_ref = {{1, 35, 1}};

  ::hkl_smatrix_times_svector(&m, &v);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&v_ref, &v));
}

void
vectorMatrixTest::hkl_smatrix_times_smatrix(void)
{
  hkl_smatrix m_ref = {{{37., 14., 13.},
      {45., 65.,  5.},
      {17.,  1., 16.}}
  };

  hkl_smatrix m = {{{ 1., 3.,-2.},
      {10., 5., 5.},
      {-3., 2., 0.}}
  };

  ::hkl_smatrix_times_smatrix(&m, &m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
vectorMatrixTest::hkl_smatrix_transpose(void)
{
  hkl_smatrix m_ref = {{{37., 14., 13.},
      {45., 65.,  5.},
      {17.,  1., 16.}}
  };

  hkl_smatrix m = {{{37., 45., 17.},
      {14., 65.,  1.},
      {13.,  5., 16.}}
  };

  ::hkl_smatrix_transpose(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}
