#include <sstream>
#include "geometry_eulerian6C_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryEulerian6CTest );

void
GeometryEulerian6CTest::setUp(void)
{
  _geometry = new hkl::eulerian6C::Geometry;
}

void
GeometryEulerian6CTest::tearDown(void)
{
  delete _geometry;
}

void
GeometryEulerian6CTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(*_geometry, *_geometry);
}

void
GeometryEulerian6CTest::copyConstructor(void)
{
  hkl::eulerian6C::Geometry geometry(*_geometry);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryEulerian6CTest::otherConstructors(void)
{
  hkl::Value mu(9 * HKL_DEGTORAD);
  hkl::Value omega(10 * HKL_DEGTORAD);
  hkl::Value chi(11 * HKL_DEGTORAD);
  hkl::Value phi(12 * HKL_DEGTORAD);

  hkl::Value gamma(13 * HKL_DEGTORAD);
  hkl::Value delta(14 * HKL_DEGTORAD);

  hkl::eulerian6C::Geometry geometry(mu.get_value(), omega.get_value(), chi.get_value(), phi.get_value(),
                                     gamma.get_value(), delta.get_value());

  // check the current part
  CPPUNIT_ASSERT_EQUAL(mu, geometry.get_axe("mu")->get_current());
  CPPUNIT_ASSERT_EQUAL(omega, geometry.get_axe("omega")->get_current());
  CPPUNIT_ASSERT_EQUAL(chi, geometry.get_axe("chi")->get_current());
  CPPUNIT_ASSERT_EQUAL(phi, geometry.get_axe("phi")->get_current());
  CPPUNIT_ASSERT_EQUAL(gamma, geometry.get_axe("gamma")->get_current());
  CPPUNIT_ASSERT_EQUAL(delta, geometry.get_axe("delta")->get_current());

  // check the consign part
  CPPUNIT_ASSERT_EQUAL(mu, geometry.get_axe("mu")->get_consign());
  CPPUNIT_ASSERT_EQUAL(omega, geometry.get_axe("omega")->get_consign());
  CPPUNIT_ASSERT_EQUAL(chi, geometry.get_axe("chi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(phi, geometry.get_axe("phi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(gamma, geometry.get_axe("gamma")->get_consign());
  CPPUNIT_ASSERT_EQUAL(delta, geometry.get_axe("delta")->get_consign());
}

void
GeometryEulerian6CTest::setAngles(void)
{
  hkl::Value mu(9 * HKL_DEGTORAD);
  hkl::Value omega(10 * HKL_DEGTORAD);
  hkl::Value chi(11 * HKL_DEGTORAD);
  hkl::Value phi(12 * HKL_DEGTORAD);

  hkl::Value gamma(13 * HKL_DEGTORAD);
  hkl::Value delta(14 * HKL_DEGTORAD);

  _geometry->set_angles(mu.get_value(), omega.get_value(), chi.get_value(), phi.get_value(),
                        gamma.get_value(), delta.get_value());
  CPPUNIT_ASSERT_EQUAL(mu, _geometry->get_axe("mu")->get_current());
  CPPUNIT_ASSERT_EQUAL(omega, _geometry->get_axe("omega")->get_current());
  CPPUNIT_ASSERT_EQUAL(chi, _geometry->get_axe("chi")->get_current());
  CPPUNIT_ASSERT_EQUAL(phi, _geometry->get_axe("phi")->get_current());
  CPPUNIT_ASSERT_EQUAL(gamma, _geometry->get_axe("gamma")->get_current());
  CPPUNIT_ASSERT_EQUAL(delta, _geometry->get_axe("delta")->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(), _geometry->get_axe("mu")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(), _geometry->get_axe("omega")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(), _geometry->get_axe("chi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(), _geometry->get_axe("phi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(), _geometry->get_axe("gamma")->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(), _geometry->get_axe("delta")->get_consign());

  _geometry->set_angles_consign(mu.get_value(), omega.get_value(), chi.get_value(), phi.get_value(),
                                gamma.get_value(), delta.get_value());
  CPPUNIT_ASSERT_EQUAL(mu, _geometry->get_axe("mu")->get_current());
  CPPUNIT_ASSERT_EQUAL(omega, _geometry->get_axe("omega")->get_current());
  CPPUNIT_ASSERT_EQUAL(chi, _geometry->get_axe("chi")->get_current());
  CPPUNIT_ASSERT_EQUAL(phi, _geometry->get_axe("phi")->get_current());
  CPPUNIT_ASSERT_EQUAL(gamma, _geometry->get_axe("gamma")->get_current());
  CPPUNIT_ASSERT_EQUAL(delta, _geometry->get_axe("delta")->get_current());
  CPPUNIT_ASSERT_EQUAL(mu, _geometry->get_axe("mu")->get_consign());
  CPPUNIT_ASSERT_EQUAL(omega, _geometry->get_axe("omega")->get_consign());
  CPPUNIT_ASSERT_EQUAL(chi, _geometry->get_axe("chi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(phi, _geometry->get_axe("phi")->get_consign());
  CPPUNIT_ASSERT_EQUAL(gamma, _geometry->get_axe("gamma")->get_consign());
  CPPUNIT_ASSERT_EQUAL(delta, _geometry->get_axe("delta")->get_consign());

}

void
GeometryEulerian6CTest::get_sample_quaternion(void)
{
  static hkl_quaternion q_ref = {{1./sqrt(2), 0, 0, 1./sqrt(2)}};
  static hkl_quaternion q_I = {{1, 0, 0, 0}};
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
GeometryEulerian6CTest::get_sample_rotation_matrix(void)
{
  static hkl_smatrix m_I = {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}};
  static hkl_smatrix m_ref = {{{0.,-1., 0.}, {1., 0., 0.}, {0., 0., 1.}}};
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
GeometryEulerian6CTest::get_Q(void)
{
  static hkl_svector svector_null = {{0, 0, 0}};
  static hkl_svector svector_X = {{1, 0, 0}};
  static hkl_svector q_ref = {{-.5, .5, sqrt(2.)/2.}};
  hkl_svector q;

  _geometry->get_axe("gamma")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  _geometry->get_source().set_ki(&svector_X);
  _geometry->get_axe("gamma")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  _geometry->get_axe("gamma")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
}

void
GeometryEulerian6CTest::get_kf(void)
{
  static hkl_svector svector_X = {{1, 0, 0}};
  static hkl_svector kf_ref = {{.5, .5, sqrt(2.)/2.}};
  static hkl_svector kf_ref2 = {{.5, -.5, -sqrt(2.)/2.}};
  hkl_svector kf;

  _geometry->get_source().set_ki(&svector_X);
  _geometry->get_axe("gamma")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_current(0. * HKL_DEGTORAD);
  _geometry->get_kf(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_X, &kf));
  _geometry->get_kf_consign(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_X, &kf));

  _geometry->get_axe("gamma")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_current(45. * HKL_DEGTORAD);
  _geometry->get_kf(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref, &kf));
  _geometry->get_kf_consign(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_X, &kf));
  _geometry->get_axe("gamma")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_kf(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref, &kf));
  _geometry->get_kf_consign(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref, &kf));

  _geometry->get_axe("gamma")->set_current(-45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_current(-45. * HKL_DEGTORAD);
  _geometry->get_kf(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref2, &kf));
  _geometry->get_kf_consign(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref, &kf));
  _geometry->get_axe("gamma")->set_consign(-45. * HKL_DEGTORAD);
  _geometry->get_axe("delta")->set_consign(-45. * HKL_DEGTORAD);
  _geometry->get_kf(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref2, &kf));
  _geometry->get_kf_consign(&kf);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&kf_ref2, &kf));
}

void
GeometryEulerian6CTest::get_distance(void)
{
  hkl::eulerian6C::Geometry g1(10 * HKL_DEGTORAD,
                               20 * HKL_DEGTORAD,
                               30 * HKL_DEGTORAD,
                               40 * HKL_DEGTORAD,
                               50 * HKL_DEGTORAD,
                               60 * HKL_DEGTORAD);

  hkl::eulerian6C::Geometry g2(11 * HKL_DEGTORAD,
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
GeometryEulerian6CTest::setFromGeometry(void)
{
  hkl::eulerian6C::Geometry E6C;
  hkl::eulerian6C::Geometry E6C_ref(0. * HKL_DEGTORAD,
                                    -80. * HKL_DEGTORAD,
                                    0. * HKL_DEGTORAD,
                                    90. * HKL_DEGTORAD,
                                    0. * HKL_DEGTORAD,
                                    40. * HKL_DEGTORAD);
  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(-80. * HKL_DEGTORAD,
                                           0. * HKL_DEGTORAD,
                                           90. * HKL_DEGTORAD,
                                           40. * HKL_DEGTORAD);
  E6C.setFromGeometry(E4CV, true);
  CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);

  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(50. * HKL_DEGTORAD, // alpha
                                        10. * HKL_DEGTORAD,
                                        0. * HKL_DEGTORAD,
                                        0. * HKL_DEGTORAD,
                                        40. * HKL_DEGTORAD);
  E6C.setFromGeometry(K4CV, true);
  CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(50 * HKL_DEGTORAD);
  E6C.setFromGeometry(K6C, true);
  E6C_ref.get_axe("omega")->set_current(-90 * HKL_DEGTORAD);
  E6C_ref.get_axe("delta")->set_current(0 * HKL_DEGTORAD);
  E6C_ref.get_axe("omega")->set_consign(-90 * HKL_DEGTORAD);
  E6C_ref.get_axe("delta")->set_consign(0 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);
}
