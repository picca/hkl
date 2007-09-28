#include <sstream>
#include "geometry_kappa6C_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa6CTest );

void
GeometryKappa6CTest::setUp(void)
{
  _alpha = 50 * HKL_DEGTORAD;
  _geometry = new hkl::kappa6C::Geometry(_alpha);
}

void
GeometryKappa6CTest::tearDown(void)
{
  delete _geometry;
}

void
GeometryKappa6CTest::equal(void)
{
  hkl::kappa6C::Geometry geometry(_alpha);
  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryKappa6CTest::copyConstructor(void)
{
  hkl::kappa6C::Geometry geometry(*_geometry);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryKappa6CTest::otherConstructors(void)
{
  double mu = 9 * HKL_DEGTORAD;
  double komega = 10 * HKL_DEGTORAD;
  double kappa = 11 * HKL_DEGTORAD;
  double kphi = 12 * HKL_DEGTORAD;

  double gamma =13 * HKL_DEGTORAD;
  double delta =14 * HKL_DEGTORAD;

  hkl::kappa6C::Geometry geometry_ref(_alpha);
  hkl::kappa6C::Geometry geometry(_alpha, mu, komega, kappa, kphi, gamma, delta);

  geometry_ref.get_axe("mu")->set_current(mu);
  geometry_ref.get_axe("komega")->set_current(komega);
  geometry_ref.get_axe("kappa")->set_current(kappa);
  geometry_ref.get_axe("kphi")->set_current(kphi);
  geometry_ref.get_axe("gamma")->set_current(gamma);
  geometry_ref.get_axe("delta")->set_current(delta);
  CPPUNIT_ASSERT_EQUAL(false, geometry_ref == geometry);

  geometry_ref.get_axe("mu")->set_consign(mu);
  geometry_ref.get_axe("komega")->set_consign(komega);
  geometry_ref.get_axe("kappa")->set_consign(kappa);
  geometry_ref.get_axe("kphi")->set_consign(kphi);
  geometry_ref.get_axe("gamma")->set_consign(gamma);
  geometry_ref.get_axe("delta")->set_consign(delta);
  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa6CTest::get_sample_quaternion(void)
{
  static hkl_quaternion q_I = {{1,0,0,0}};
  static hkl_quaternion q_ref = {{1./sqrt(2), 0, 0, 1./sqrt(2)}};
  hkl_quaternion q;

  _geometry->get_axe("mu")->set_current(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_I, &q));

  _geometry->get_axe("mu")->set_consign(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
GeometryKappa6CTest::get_sample_rotation_matrix(void)
{
  static hkl_smatrix m_ref = {{{0,-1,0}, {1,0,0}, {0,0,1}}};
  static hkl_smatrix m_I = {{{1,0,0}, {0,1,0}, {0,0,1}}};
  hkl_smatrix m;

  _geometry->get_axe("mu")->set_current(90. * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_I, &m));

  _geometry->get_axe("mu")->set_consign(90. * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
GeometryKappa6CTest::get_Q(void)
{
  static hkl_svector svector_null = {{0,0,0}};
  static hkl_svector svector_X = {{1,0,0}};
  static hkl_svector Q_ref = {{-.5, .5, sqrt(2.)/2.}};
  hkl_svector Q;

  _geometry->get_axe("gamma")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_Q(&Q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &Q));
  _geometry->get_Q_consign(&Q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &Q));

  _geometry->get_source().set_ki(&svector_X);
  _geometry->get_axe("gamma")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_Q(&Q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&Q_ref, &Q));
  _geometry->get_Q_consign(&Q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &Q));

  _geometry->get_axe("gamma")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_Q(&Q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&Q_ref, &Q));
  _geometry->get_Q_consign(&Q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&Q_ref, &Q));
}

void
GeometryKappa6CTest::get_distance(void)
{
  hkl::kappa6C::Geometry g1(_alpha,
                            10 * HKL_DEGTORAD,
                            20 * HKL_DEGTORAD,
                            30 * HKL_DEGTORAD,
                            40 * HKL_DEGTORAD,
                            50 * HKL_DEGTORAD,
                            60 * HKL_DEGTORAD);

  hkl::kappa6C::Geometry g2(_alpha,
                            11 * HKL_DEGTORAD,
                            21 * HKL_DEGTORAD,
                            31 * HKL_DEGTORAD,
                            41 * HKL_DEGTORAD,
                            51 * HKL_DEGTORAD,
                            61 * HKL_DEGTORAD);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  g2.set_angles(10 * HKL_DEGTORAD,
                20 * HKL_DEGTORAD,
                30 * HKL_DEGTORAD,
                40 * HKL_DEGTORAD,
                50 * HKL_DEGTORAD,
                60 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  g2.set_angles_consign(10 * HKL_DEGTORAD,
                        20 * HKL_DEGTORAD,
                        30 * HKL_DEGTORAD,
                        40 * HKL_DEGTORAD,
                        50 * HKL_DEGTORAD,
                        60 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);
}

void
GeometryKappa6CTest::setFromGeometry(void)
{
  hkl::kappa6C::Geometry K6C(_alpha);
  hkl::kappa6C::Geometry K6C_ref(_alpha,
                                 0. * HKL_DEGTORAD,
                                 0. * HKL_DEGTORAD,
                                 0. * HKL_DEGTORAD,
                                 0. * HKL_DEGTORAD,
                                 0. * HKL_DEGTORAD,
                                 40. * HKL_DEGTORAD);

  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(-90. * HKL_DEGTORAD,
                                           0. * HKL_DEGTORAD,
                                           90. * HKL_DEGTORAD,
                                           40. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E4CV, false));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);

  E4CV.get_axe("chi")->set_current(110 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E4CV, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E4CV, false), hkl::HKLException);

  //Eulerian6C
  hkl::eulerian6C::Geometry E6C(0. * HKL_DEGTORAD,
                                -90. * HKL_DEGTORAD,
                                0. * HKL_DEGTORAD,
                                90. * HKL_DEGTORAD,
                                0. * HKL_DEGTORAD,
                                40. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E6C, true));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);

  E6C.get_axe("chi")->set_current(110 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E6C, false), hkl::HKLException);
}

void
GeometryKappa6CTest::persistanceIO(void)
{
  hkl::kappa6C::Geometry geometry1(_alpha);
  hkl::kappa6C::Geometry geometry2(_alpha);
  std::stringstream flux;

  _geometry->get_axe("komega")->set_current(2.);
  _geometry->toStream(flux);
  _geometry->get_axe("komega")->set_current(3.);
  _geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  _geometry->get_axe("komega")->set_current(2.);
  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry1);
  _geometry->get_axe("komega")->set_current(3.);
  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry2);
}
