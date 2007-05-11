#include <sstream>
#include "geometry_eulerian6C_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryEulerian6CTest );

void
GeometryEulerian6CTest::setUp(void)
{
  m_geometry = hkl::eulerian6C::Geometry();
}

void
GeometryEulerian6CTest::tearDown(void)
{}

void
GeometryEulerian6CTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(m_geometry, m_geometry);
}

void
GeometryEulerian6CTest::copyConstructor(void)
{
  hkl::eulerian6C::Geometry geometry(m_geometry);

  CPPUNIT_ASSERT_EQUAL(m_geometry, geometry);
}

void
GeometryEulerian6CTest::otherConstructors(void)
{
  double mu = 9 * hkl::constant::math::degToRad;
  double omega = 10 * hkl::constant::math::degToRad;
  double chi = 11 * hkl::constant::math::degToRad;
  double phi = 12 * hkl::constant::math::degToRad;

  double gamma = 13 * hkl::constant::math::degToRad;
  double delta = 14 * hkl::constant::math::degToRad;

  m_geometry = hkl::eulerian6C::Geometry(mu, omega, chi, phi, gamma, delta);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(mu, m_geometry.get_axe("mu").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(chi, m_geometry.get_axe("chi").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(phi, m_geometry.get_axe("phi").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(gamma, m_geometry.get_axe("gamma").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(delta, m_geometry.get_axe("delta").get_current().get_value(), hkl::constant::math::epsilon);
}

void
GeometryEulerian6CTest::setAngles(void)
{
  double mu = 9 * hkl::constant::math::degToRad;
  double omega = 10 * hkl::constant::math::degToRad;
  double chi = 11 * hkl::constant::math::degToRad;
  double phi = 12 * hkl::constant::math::degToRad;

  double gamma = 13 * hkl::constant::math::degToRad;
  double delta = 14 * hkl::constant::math::degToRad;

  m_geometry.setAngles(mu, omega, chi, phi, gamma, delta);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(mu, m_geometry.get_axe("mu").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(chi, m_geometry.get_axe("chi").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(phi, m_geometry.get_axe("phi").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(gamma, m_geometry.get_axe("gamma").get_current().get_value(), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(delta, m_geometry.get_axe("delta").get_current().get_value(), hkl::constant::math::epsilon);
}

void
GeometryEulerian6CTest::getAxesNames(void)
{
  vector<string> v = m_geometry.getAxesNames();
  CPPUNIT_ASSERT_EQUAL(string("mu"), v[0]);
  CPPUNIT_ASSERT_EQUAL(string("omega"), v[1]);
  CPPUNIT_ASSERT_EQUAL(string("chi"), v[2]);
  CPPUNIT_ASSERT_EQUAL(string("phi"), v[3]);
  CPPUNIT_ASSERT_EQUAL(string("gamma"), v[4]);
  CPPUNIT_ASSERT_EQUAL(string("delta"), v[5]);
}

void
GeometryEulerian6CTest::getSampleQuaternion(void)
{
  m_geometry.get_axe("mu").set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry.getSampleQuaternion());
}

void
GeometryEulerian6CTest::getSampleRotationMatrix(void)
{
  m_geometry.get_axe("mu").set_current(90. * hkl::constant::math::degToRad);

  hkl::smatrix M( 0.,-1., 0.,
             1., 0., 0.,
             0., 0., 1.);

  CPPUNIT_ASSERT_EQUAL(M, m_geometry.getSampleRotationMatrix());
}

void
GeometryEulerian6CTest::getQ(void)
{
  m_geometry.get_axe("gamma").set_current(0. * hkl::constant::math::degToRad);
  m_geometry.get_axe("delta").set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), m_geometry.getQ());

  m_geometry.get_source().setKi(hkl::svector(1, 0, 0));
  m_geometry.get_axe("gamma").set_current(45. * hkl::constant::math::degToRad);
  m_geometry.get_axe("delta").set_current(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(-.5, .5, sqrt(2.)/2.), m_geometry.getQ());
}

void
GeometryEulerian6CTest::getKf(void)
{
  m_geometry.get_source().setKi(hkl::svector(1, 0, 0));
  m_geometry.get_axe("gamma").set_current(0. * hkl::constant::math::degToRad);
  m_geometry.get_axe("delta").set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1., 0., 0.), m_geometry.getKf());

  m_geometry.get_axe("gamma").set_current(45. * hkl::constant::math::degToRad);
  m_geometry.get_axe("delta").set_current(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(.5, .5, sqrt(2.)/2.), m_geometry.getKf());

  m_geometry.get_axe("gamma").set_current(-45. * hkl::constant::math::degToRad);
  m_geometry.get_axe("delta").set_current(-45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(.5, -.5, -sqrt(2.)/2.), m_geometry.getKf());
}

void
GeometryEulerian6CTest::getDistance(void)
{
  hkl::eulerian6C::Geometry g1(10 * hkl::constant::math::degToRad,
                          20 * hkl::constant::math::degToRad,
                          30 * hkl::constant::math::degToRad,
                          40 * hkl::constant::math::degToRad,
                          50 * hkl::constant::math::degToRad,
                          60 * hkl::constant::math::degToRad);

  hkl::eulerian6C::Geometry g2(11 * hkl::constant::math::degToRad,
                          21 * hkl::constant::math::degToRad,
                          31 * hkl::constant::math::degToRad,
                          41 * hkl::constant::math::degToRad,
                          51 * hkl::constant::math::degToRad,
                          61 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * hkl::constant::math::degToRad, g1.getDistance(g2), hkl::constant::math::epsilon);

  g2.get_axe("mu").set_current(10 * hkl::constant::math::degToRad);
  g2.get_axe("omega").set_current(20 * hkl::constant::math::degToRad);
  g2.get_axe("chi").set_current(30 * hkl::constant::math::degToRad);
  g2.get_axe("phi").set_current(40 * hkl::constant::math::degToRad);
  g2.get_axe("gamma").set_current(50 * hkl::constant::math::degToRad);
  g2.get_axe("delta").set_current(60 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.getDistance(g2), hkl::constant::math::epsilon);
}

void
GeometryEulerian6CTest::setFromGeometry(void)
{
  hkl::eulerian6C::Geometry E6C;
  hkl::eulerian6C::Geometry E6C_ref(0. * hkl::constant::math::degToRad,
                               100. * hkl::constant::math::degToRad,
                               0. * hkl::constant::math::degToRad,
                               -90. * hkl::constant::math::degToRad,
                               0. * hkl::constant::math::degToRad,
                               40. * hkl::constant::math::degToRad);
  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(100. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           -90. * hkl::constant::math::degToRad,
                                           40. * hkl::constant::math::degToRad);
  E6C.setFromGeometry(E4CV, true);
  CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);

  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(10. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        40. * hkl::constant::math::degToRad);
  E6C.setFromGeometry(K4CV, true);
  CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);

  //Kappa6C
  hkl::kappa6C::Geometry K6C;
  E6C.setFromGeometry(K6C, true);
  E6C_ref.get_axe("omega").set_current(90 * hkl::constant::math::degToRad);
  E6C_ref.get_axe("delta").set_current(0 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(E6C_ref, E6C);
}

void
GeometryEulerian6CTest::persistanceIO(void)
{
    hkl::eulerian6C::Geometry geometry1;
    hkl::eulerian6C::Geometry geometry2;
    stringstream flux;

    m_geometry.toStream(flux);
    m_geometry.toStream(flux);
    geometry1.fromStream(flux);
    geometry2.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry1);
    CPPUNIT_ASSERT_EQUAL(m_geometry, geometry2);
}
