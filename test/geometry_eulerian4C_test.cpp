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
  double omega = 10 * hkl::constant::math::degToRad;
  double chi = 11 * hkl::constant::math::degToRad;
  double phi = 12 * hkl::constant::math::degToRad;
  double two_theta = 13 * hkl::constant::math::degToRad;

  hkl::eulerian4C::vertical::Geometry geometry_ref;
  hkl::eulerian4C::vertical::Geometry geometry(omega, chi, phi, two_theta);

  geometry_ref.get_axe("omega")->set_current(omega);
  geometry_ref.get_axe("chi")->set_current(chi);
  geometry_ref.get_axe("phi")->set_current(phi);
  geometry_ref.get_axe("tth")->set_current(two_theta);

  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryEulerian4CTest::getSampleQuaternion(void)
{
  _geometry->get_axe("omega")->set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), _geometry->getSampleQuaternion());
}

void
GeometryEulerian4CTest::getSampleRotationMatrix(void)
{
  _geometry->get_axe("omega")->set_current(90. * hkl::constant::math::degToRad);

  hkl::smatrix M( 0., 0.,-1.,
                  0., 1., 0.,
                  1., 0., 0.);

  CPPUNIT_ASSERT_EQUAL(M, _geometry->getSampleRotationMatrix());
}

void
GeometryEulerian4CTest::getQ(void)
{
  _geometry->get_source().setKi(hkl::svector(1, 0, 0));

  _geometry->get_axe("tth")->set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), _geometry->getQ());

  _geometry->get_axe("tth")->set_current(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1./sqrt(2)-1., 0, sqrt(2.)/2.), _geometry->getQ());
}

void
GeometryEulerian4CTest::get_distance(void)
{
  hkl::eulerian4C::vertical::Geometry g1(10 * hkl::constant::math::degToRad,
                                    20 * hkl::constant::math::degToRad,
                                    30 * hkl::constant::math::degToRad,
                                    40 * hkl::constant::math::degToRad);

  hkl::eulerian4C::vertical::Geometry g2(11 * hkl::constant::math::degToRad,
                                    21 * hkl::constant::math::degToRad,
                                    31 * hkl::constant::math::degToRad,
                                    41 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);

  g2.get_axe("omega")->set_current(10 * hkl::constant::math::degToRad);
  g2.get_axe("chi")->set_current(20 * hkl::constant::math::degToRad);
  g2.get_axe("phi")->set_current(30 * hkl::constant::math::degToRad);
  g2.get_axe("tth")->set_current(40 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
}

void
GeometryEulerian4CTest::setFromGeometry(void)
{
  hkl::eulerian4C::vertical::Geometry E4CV;
  hkl::eulerian4C::vertical::Geometry E4CV_ref(100. * hkl::constant::math::degToRad,
                                          0. * hkl::constant::math::degToRad,
                                          -90. * hkl::constant::math::degToRad,
                                          40. * hkl::constant::math::degToRad);
  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(50. * hkl::constant::math::degToRad, // alpha
                                        10. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        40. * hkl::constant::math::degToRad);
  E4CV.setFromGeometry(K4CV, true);
  CPPUNIT_ASSERT_EQUAL(E4CV_ref, E4CV);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(50 * hkl::constant::math::degToRad);
  E4CV.setFromGeometry(K6C, true);
  E4CV_ref.get_axe("omega")->set_current(90 * hkl::constant::math::degToRad);
  E4CV_ref.get_axe("tth")->set_current(0 * hkl::constant::math::degToRad);
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

void
GeometryEulerian4CTest::persistanceIO(void)
{
  hkl::eulerian4C::vertical::Geometry geometry1;
  hkl::eulerian4C::vertical::Geometry geometry2;
  std::stringstream flux;

  _geometry->toStream(flux);
  _geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry2);
}
