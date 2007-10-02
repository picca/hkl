#include <iostream>
#include "geometry_kappa4C_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa4CTest );

void
GeometryKappa4CTest::setUp(void)
{
  _alpha = 50 * HKL_DEGTORAD;
  _geometry = new hkl::kappa4C::vertical::Geometry(_alpha);
}

void
GeometryKappa4CTest::tearDown(void)
{
  delete _geometry;
}

void
GeometryKappa4CTest::equal(void)
{
  hkl::kappa4C::vertical::Geometry geometry(_alpha);
  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryKappa4CTest::copyConstructor(void)
{
  hkl::kappa4C::vertical::Geometry geometry(*_geometry);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryKappa4CTest::otherConstructors(void)
{
  double komega = 10 * HKL_DEGTORAD;
  double kappa = 11 * HKL_DEGTORAD;
  double kphi = 12 * HKL_DEGTORAD;
  double two_theta = 13 * HKL_DEGTORAD;

  hkl::kappa4C::vertical::Geometry geometry_ref(_alpha);
  hkl::kappa4C::vertical::Geometry geometry(_alpha, komega, kappa, kphi, two_theta);

  geometry_ref.get_axe("komega")->set_current(komega);
  geometry_ref.get_axe("kappa")->set_current(kappa);
  geometry_ref.get_axe("kphi")->set_current(kphi);
  geometry_ref.get_axe("tth")->set_current(two_theta);
  CPPUNIT_ASSERT_EQUAL(false, geometry_ref == geometry);
  geometry_ref.get_axe("komega")->set_consign(komega);
  geometry_ref.get_axe("kappa")->set_consign(kappa);
  geometry_ref.get_axe("kphi")->set_consign(kphi);
  geometry_ref.get_axe("tth")->set_consign(two_theta);
  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa4CTest::get_alpha(void)
{
  CPPUNIT_ASSERT_EQUAL(_alpha, _geometry->get_alpha());
}

void
GeometryKappa4CTest::get_sample_quaternion(void)
{
  static hkl_quaternion q_ref = {{1./sqrt(2), 0, -1./sqrt(2), 0.}};
  static hkl_quaternion q_I = {{1, 0, 0, 0}};
  hkl_quaternion q;

  _geometry->get_axe("komega")->set_current(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_I, &q));

  _geometry->get_axe("komega")->set_consign(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
GeometryKappa4CTest::get_sample_rotation_matrix(void)
{
  static hkl_smatrix m_ref = {{{0,0,-1}, {0,1,0}, {1,0,0}}};
  static hkl_smatrix m_I = {{{1,0,0}, {0,1,0}, {0,0,1}}};
  hkl_smatrix m;

  _geometry->get_axe("komega")->set_current(90. * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_I, &m));

  _geometry->get_axe("komega")->set_consign(90. * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
GeometryKappa4CTest::get_Q(void)
{
  static hkl_svector svector_null = {{0,0,0}};
  static hkl_svector svector_X = {{1,0,0}};
  static hkl_svector q_ref = {{1./sqrt(2)-1., 0, 1./sqrt(2.)}};
  hkl_svector q;

  _geometry->get_axe("tth")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  _geometry->source.wave_length = HKL_TAU;
  _geometry->source.direction = svector_X;

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
GeometryKappa4CTest::get_distance(void)
{
  hkl::kappa4C::vertical::Geometry g1(_alpha,
                                      10 * HKL_DEGTORAD,
                                      20 * HKL_DEGTORAD,
                                      30 * HKL_DEGTORAD,
                                      40 * HKL_DEGTORAD);

  hkl::kappa4C::vertical::Geometry g2(_alpha,
                                      11 * HKL_DEGTORAD,
                                      21 * HKL_DEGTORAD,
                                      31 * HKL_DEGTORAD,
                                      41 * HKL_DEGTORAD);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  g2.get_axe("komega")->set_current(10 * HKL_DEGTORAD);
  g2.get_axe("kappa")->set_current(20 * HKL_DEGTORAD);
  g2.get_axe("kphi")->set_current(30 * HKL_DEGTORAD);
  g2.get_axe("tth")->set_current(40 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  g2.get_axe("komega")->set_consign(10 * HKL_DEGTORAD);
  g2.get_axe("kappa")->set_consign(20 * HKL_DEGTORAD);
  g2.get_axe("kphi")->set_consign(30 * HKL_DEGTORAD);
  g2.get_axe("tth")->set_consign(40 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);
}

void
GeometryKappa4CTest::set_angles(void)
{
  hkl::Value komega(10 * HKL_DEGTORAD);
  hkl::Value kappa(20 * HKL_DEGTORAD);
  hkl::Value kphi(30 * HKL_DEGTORAD);
  hkl::Value tth(40 * HKL_DEGTORAD);

  _geometry->set_angles(komega.get_value(), kappa.get_value(), kphi.get_value(), tth.get_value());
  CPPUNIT_ASSERT_EQUAL(komega, _geometry->get_axe("komega")->get_current());
  CPPUNIT_ASSERT_EQUAL(kappa, _geometry->get_axe("kappa")->get_current());
  CPPUNIT_ASSERT_EQUAL(kphi, _geometry->get_axe("kphi")->get_current());
  CPPUNIT_ASSERT_EQUAL(tth, _geometry->get_axe("tth")->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0.), _geometry->get_axe("komega")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0.), _geometry->get_axe("kappa")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0.), _geometry->get_axe("kphi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(0.), _geometry->get_axe("tth")->get_consign());

  _geometry->set_angles_consign(komega.get_value(), kappa.get_value(), kphi.get_value(), tth.get_value());
  CPPUNIT_ASSERT_EQUAL(komega, _geometry->get_axe("komega")->get_current());
  CPPUNIT_ASSERT_EQUAL(kappa, _geometry->get_axe("kappa")->get_current());
  CPPUNIT_ASSERT_EQUAL(kphi, _geometry->get_axe("kphi")->get_current());
  CPPUNIT_ASSERT_EQUAL(tth, _geometry->get_axe("tth")->get_current());
  CPPUNIT_ASSERT_EQUAL(komega, _geometry->get_axe("komega")->get_consign());
  CPPUNIT_ASSERT_EQUAL(kappa, _geometry->get_axe("kappa")->get_consign());
  CPPUNIT_ASSERT_EQUAL(kphi, _geometry->get_axe("kphi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(tth, _geometry->get_axe("tth")->get_consign());
}

void
GeometryKappa4CTest::setFromGeometry(void)
{
  hkl::kappa4C::vertical::Geometry K4CV(_alpha);
  hkl::kappa4C::vertical::Geometry K4CV_ref(_alpha,
                                            0. * HKL_DEGTORAD,
                                            0. * HKL_DEGTORAD,
                                            0. * HKL_DEGTORAD,
                                            40. * HKL_DEGTORAD);

  //eulerian4C::Vertical
  // axes limits are all set to [-pi:pi]
  hkl::eulerian4C::vertical::Geometry E4CV(-90. * HKL_DEGTORAD,
                                           0. * HKL_DEGTORAD,
                                           90. * HKL_DEGTORAD,
                                           40. * HKL_DEGTORAD);

  // no more exception if the chi range is [-2*alpha:2*alpha]
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E4CV, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  E4CV.get_axe("chi")->set_current(2.1 * _alpha);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E4CV, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E4CV, false), hkl::HKLException);

  //Eulerian6C
  hkl::eulerian6C::Geometry E6C( 0. * HKL_DEGTORAD,
                                 -90. * HKL_DEGTORAD,
                                 0. * HKL_DEGTORAD,
                                 90. * HKL_DEGTORAD,
                                 0. * HKL_DEGTORAD,
                                 40. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  E6C.get_axe("mu")->set_current(1 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("gamma")->set_current(1 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("mu")->set_current(0 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("chi")->set_current(110 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, false), hkl::HKLException);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(_alpha,
                             0 * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             40. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  K6C.get_axe("mu")->set_current(1 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  K6C.get_axe("gamma")->set_current(1 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  K6C.get_axe("mu")->set_current(0 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);


}
