#include <iostream>
#include "geometry_kappa4C_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa4CTest );

void
GeometryKappa4CTest::setUp(void)
{
  m_alpha = 50 * hkl::constant::math::degToRad;
  m_geometry = new hkl::kappa4C::vertical::Geometry(m_alpha);
}

void
GeometryKappa4CTest::tearDown(void)
{
  delete m_geometry;
}

void
GeometryKappa4CTest::equal(void)
{
  hkl::kappa4C::vertical::Geometry geometry(m_alpha);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa4CTest::copyConstructor(void)
{
  hkl::kappa4C::vertical::Geometry geometry(*m_geometry);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa4CTest::otherConstructors(void)
{
  double komega = 10 * hkl::constant::math::degToRad;
  double kappa = 11 * hkl::constant::math::degToRad;
  double kphi = 12 * hkl::constant::math::degToRad;
  double two_theta = 13 * hkl::constant::math::degToRad;

  hkl::kappa4C::vertical::Geometry geometry_ref(m_alpha);
  hkl::kappa4C::vertical::Geometry geometry(m_alpha, komega, kappa, kphi, two_theta);

  geometry_ref.get_axe("komega")->set_current(komega);
  geometry_ref.get_axe("kappa")->set_current(kappa);
  geometry_ref.get_axe("kphi")->set_current(kphi);
  geometry_ref.get_axe("tth")->set_current(two_theta);

  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa4CTest::get_alpha(void)
{
  CPPUNIT_ASSERT_EQUAL(m_alpha, m_geometry->get_alpha());
}

void
GeometryKappa4CTest::getSampleQuaternion(void)
{
  m_geometry->get_axe("komega")->set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), m_geometry->getSampleQuaternion());
}

void
GeometryKappa4CTest::getSampleRotationMatrix(void)
{
  m_geometry->get_axe("komega")->set_current(90. * hkl::constant::math::degToRad);

  hkl::smatrix M( 0., 0.,-1.,
             0., 1., 0.,
             1., 0., 0.);

  CPPUNIT_ASSERT_EQUAL(M, m_geometry->getSampleRotationMatrix());
}

void
GeometryKappa4CTest::getQ(void)
{
  m_geometry->get_axe("tth")->set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), m_geometry->getQ());

  m_geometry->get_source().setKi(hkl::svector(1, 0, 0));
  m_geometry->get_axe("tth")->set_current(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1./sqrt(2)-1., 0, 1./sqrt(2.)), m_geometry->getQ());
}

void
GeometryKappa4CTest::get_distance(void)
{
  hkl::kappa4C::vertical::Geometry g1(m_alpha,
                                      10 * hkl::constant::math::degToRad,
                                      20 * hkl::constant::math::degToRad,
                                      30 * hkl::constant::math::degToRad,
                                      40 * hkl::constant::math::degToRad);

  hkl::kappa4C::vertical::Geometry g2(m_alpha,
                                      11 * hkl::constant::math::degToRad,
                                      21 * hkl::constant::math::degToRad,
                                      31 * hkl::constant::math::degToRad,
                                      41 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);

  g2.get_axe("komega")->set_current(10 * hkl::constant::math::degToRad);
  g2.get_axe("kappa")->set_current(20 * hkl::constant::math::degToRad);
  g2.get_axe("kphi")->set_current(30 * hkl::constant::math::degToRad);
  g2.get_axe("tth")->set_current(40 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
}

void
GeometryKappa4CTest::setAngles(void)
{
  double komega = 10 * hkl::constant::math::degToRad;
  double kappa = 20 * hkl::constant::math::degToRad;
  double kphi = 30 * hkl::constant::math::degToRad;
  double two_theta = 40 * hkl::constant::math::degToRad;

  m_geometry->setAngles(komega, kappa, kphi, two_theta);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(komega, m_geometry->get_axe("komega")->get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(kappa, m_geometry->get_axe("kappa")->get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(kphi, m_geometry->get_axe("kphi")->get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(two_theta, m_geometry->get_axe("tth")->get_current().get_value(), hkl::constant::math::epsilon);
}

void
GeometryKappa4CTest::setFromGeometry(void)
{
  hkl::kappa4C::vertical::Geometry K4CV(m_alpha);
  hkl::kappa4C::vertical::Geometry K4CV_ref(m_alpha,
                                            0. * hkl::constant::math::degToRad,
                                            0. * hkl::constant::math::degToRad,
                                            0. * hkl::constant::math::degToRad,
                                            40. * hkl::constant::math::degToRad);

  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(90. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           -90. * hkl::constant::math::degToRad,
                                           40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E4CV, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  E4CV.get_axe("chi")->set_current(110 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E4CV, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E4CV, false), hkl::HKLException);

  //Eulerian6C
  hkl::eulerian6C::Geometry E6C( 0. * hkl::constant::math::degToRad,
                                90. * hkl::constant::math::degToRad,
                                0. * hkl::constant::math::degToRad,
                                -90. * hkl::constant::math::degToRad,
                                0. * hkl::constant::math::degToRad,
                                40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  E6C.get_axe("mu")->set_current(1 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("gamma")->set_current(1 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("mu")->set_current(0 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("chi")->set_current(110 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, false), hkl::HKLException);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(m_alpha,
                             0 * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  K6C.get_axe("mu")->set_current(1 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  K6C.get_axe("gamma")->set_current(1 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  K6C.get_axe("mu")->set_current(0 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);


}

void
GeometryKappa4CTest::persistanceIO(void)
{
  hkl::kappa4C::vertical::Geometry geometry1(m_alpha);
  hkl::kappa4C::vertical::Geometry geometry2(m_alpha);
  std::stringstream flux;

  m_geometry->get_axe("komega")->set_current(2.);
  m_geometry->toStream(flux);
  m_geometry->get_axe("komega")->set_current(3.);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  m_geometry->get_axe("komega")->set_current(2.);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  m_geometry->get_axe("komega")->set_current(3.);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
