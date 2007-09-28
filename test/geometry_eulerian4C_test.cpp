// File to test angleconfiguration implementation.
#include "geometry_eulerian4C_test.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryEulerian4CTest );

void
GeometryEulerian4CTest::setUp(void)
{
  _geometry = new hkl::eulerian4C::vertical::Geometry;
}

void
GeometryEulerian4CTest::tearDown(void)
{
  delete _geometry;
}

void
GeometryEulerian4CTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(*_geometry, *_geometry);
}

void
GeometryEulerian4CTest::copyConstructor(void)
{
  hkl::eulerian4C::vertical::Geometry geometry(*_geometry);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryEulerian4CTest::otherConstructors(void)
{
  double omega = 10 * HKL_DEGTORAD;
  double chi = 11 * HKL_DEGTORAD;
  double phi = 12 * HKL_DEGTORAD;
  double two_theta = 13 * HKL_DEGTORAD;

  hkl::eulerian4C::vertical::Geometry geometry_ref;
  hkl::eulerian4C::vertical::Geometry geometry(omega, chi, phi, two_theta);

  geometry_ref.get_axe("omega")->set_current(omega);
  geometry_ref.get_axe("chi")->set_current(chi);
  geometry_ref.get_axe("phi")->set_current(phi);
  geometry_ref.get_axe("tth")->set_current(two_theta);
  // the consign is not well set so the test must fail
  CPPUNIT_ASSERT_EQUAL(false, geometry_ref == geometry);

  geometry_ref.get_axe("omega")->set_consign(omega);
  geometry_ref.get_axe("chi")->set_consign(chi);
  geometry_ref.get_axe("phi")->set_consign(phi);
  geometry_ref.get_axe("tth")->set_consign(two_theta);
  // now it must be ok.
  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryEulerian4CTest::get_sample_quaternion(void)
{
  static hkl_quaternion q_ref = {{1./sqrt(2), 0, -1./sqrt(2), 0.}};
  static hkl_quaternion q_I = {{1, 0, 0, 0}};
  hkl_quaternion q;

  _geometry->get_axe("omega")->set_current(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_I, &q));

  _geometry->get_axe("omega")->set_consign(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
GeometryEulerian4CTest::get_sample_rotation_matrix(void)
{
  static hkl_smatrix m_I = {{{1,0,0},{0,1,0},{0,0,1}}};
  static hkl_smatrix m_ref = {{{0,0,-1},{0,1,0},{1,0,0}}};
  hkl_smatrix m;

  _geometry->get_axe("omega")->set_current(90. * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_I, &m));


  _geometry->get_axe("omega")->set_consign(90. * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
GeometryEulerian4CTest::get_Q(void)
{
  static hkl_svector svector_X = {{1, 0, 0}};
  static hkl_svector svector_null = {{0, 0, 0}};
  static hkl_svector q_ref = {{1/sqrt(2)-1, 0, sqrt(2)/2}};
  hkl_svector q;

  _geometry->get_source().set_ki(&svector_X);

  _geometry->get_axe("tth")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  _geometry->get_axe("tth")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  _geometry->get_axe("tth")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
}

void
GeometryEulerian4CTest::get_distance(void)
{
  hkl::eulerian4C::vertical::Geometry g1(10 * HKL_DEGTORAD,
                                         20 * HKL_DEGTORAD,
                                         30 * HKL_DEGTORAD,
                                         40 * HKL_DEGTORAD);

  hkl::eulerian4C::vertical::Geometry g2(11 * HKL_DEGTORAD,
                                         21 * HKL_DEGTORAD,
                                         31 * HKL_DEGTORAD,
                                         41 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  // now set the current position
  g2.get_axe("omega")->set_current(10 * HKL_DEGTORAD);
  g2.get_axe("chi")->set_current(20 * HKL_DEGTORAD);
  g2.get_axe("phi")->set_current(30 * HKL_DEGTORAD);
  g2.get_axe("tth")->set_current(40 * HKL_DEGTORAD);
  // the current distance changed.
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  // not the consign distance
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  // now set the consign position
  g2.get_axe("omega")->set_consign(10 * HKL_DEGTORAD);
  g2.get_axe("chi")->set_consign(20 * HKL_DEGTORAD);
  g2.get_axe("phi")->set_consign(30 * HKL_DEGTORAD);
  g2.get_axe("tth")->set_consign(40 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);
}

void
GeometryEulerian4CTest::setFromGeometry(void)
{
  hkl::eulerian4C::vertical::Geometry E4CV;
  hkl::eulerian4C::vertical::Geometry E4CV_ref(-80. * HKL_DEGTORAD,
                                               0. * HKL_DEGTORAD,
                                               90. * HKL_DEGTORAD,
                                               40. * HKL_DEGTORAD);
  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(50. * HKL_DEGTORAD, // alpha
                                        10. * HKL_DEGTORAD,
                                        0. * HKL_DEGTORAD,
                                        0. * HKL_DEGTORAD,
                                        40. * HKL_DEGTORAD);
  E4CV.setFromGeometry(K4CV, true);
  CPPUNIT_ASSERT_EQUAL(E4CV_ref, E4CV);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(50 * HKL_DEGTORAD);
  E4CV.setFromGeometry(K6C, true);
  E4CV_ref.get_axe("omega")->set_current(-90 * HKL_DEGTORAD);
  E4CV_ref.get_axe("tth")->set_current(0 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_EQUAL(false, E4CV_ref == E4CV);

  E4CV_ref.get_axe("omega")->set_consign(-90 * HKL_DEGTORAD);
  E4CV_ref.get_axe("tth")->set_consign(0 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_EQUAL(E4CV_ref, E4CV);

  // exceptions
  K6C.get_axe("mu")->set_current(1.);
  CPPUNIT_ASSERT_THROW(E4CV.setFromGeometry(K6C, true), hkl::HKLException);
  K6C.get_axe("mu")->set_current(0.);
  K6C.get_axe("gamma")->set_current(1.);
  CPPUNIT_ASSERT_THROW(E4CV.setFromGeometry(K6C, true), hkl::HKLException);
  K6C.get_axe("mu")->set_current(1.);
  CPPUNIT_ASSERT_THROW(E4CV.setFromGeometry(K6C, true), hkl::HKLException);
}
