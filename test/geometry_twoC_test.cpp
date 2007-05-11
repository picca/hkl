#include "geometry_twoC_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTwoCTest );

void
GeometryTwoCTest::setUp(void)
{
  _geometry = hkl::twoC::vertical::Geometry();
  _lattice.a().set_current(1.54);
  _lattice.b().set_current(1.54);
  _lattice.c().set_current(1.54);
  _lattice.alpha().set_current(90 * hkl::constant::math::degToRad);
  _lattice.beta().set_current(90 * hkl::constant::math::degToRad);
  _lattice.gamma().set_current(90 * hkl::constant::math::degToRad);
  /*
      _geometry.get_source().setWaveLength(1.54);
      _geometry.m_omega.set_value(10 * hkl::constant::math::degToRad);
      _geometry.m_tth.set_value(30 * hkl::constant::math::degToRad);
      */
}

void
GeometryTwoCTest::tearDown(void)
{}

void
GeometryTwoCTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(_geometry, _geometry);
}

void
GeometryTwoCTest::copyConstructor(void)
{
  hkl::twoC::vertical::Geometry geometry(_geometry);

  CPPUNIT_ASSERT_EQUAL(_geometry, geometry);
}

void
GeometryTwoCTest::otherConstructors(void)
{
  double omega = 10 * hkl::constant::math::degToRad;
  double two_theta =13 * hkl::constant::math::degToRad;

  hkl::twoC::vertical::Geometry geometry_ref;
  hkl::twoC::vertical::Geometry geometry(omega, two_theta);

  geometry_ref.get_axe("omega").set_current(omega);
  geometry_ref.get_axe("2theta").set_current(two_theta);

  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryTwoCTest::getAxesNames(void)
{
  vector<string> v = _geometry.getAxesNames();
  CPPUNIT_ASSERT_EQUAL(string("omega"), v[0]);
  CPPUNIT_ASSERT_EQUAL(string("2theta"), v[1]);
}

void
GeometryTwoCTest::getSampleQuaternion(void)
{
  _geometry.get_axe("omega").set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), _geometry.getSampleQuaternion());
}

void
GeometryTwoCTest::getSampleRotationMatrix(void)
{
  _geometry.get_axe("omega").set_current(90. * hkl::constant::math::degToRad);

  hkl::smatrix M( 0., 0.,-1.,
             0., 1., 0.,
             1., 0., 0.);

  CPPUNIT_ASSERT_EQUAL(M, _geometry.getSampleRotationMatrix());
}

void
GeometryTwoCTest::getQ(void)
{
  _geometry.get_axe("2theta").set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), _geometry.getQ());

  _geometry.get_axe("2theta").set_current(45. * hkl::constant::math::degToRad);
  _geometry.get_source().setKi(hkl::svector(1, 0, 0));
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1./sqrt(2)-1., 0, sqrt(2.)/2.), _geometry.getQ());
}

void
GeometryTwoCTest::getDistance(void)
{
  hkl::twoC::vertical::Geometry g1(10 * hkl::constant::math::degToRad,
                              40 * hkl::constant::math::degToRad);

  hkl::twoC::vertical::Geometry g2(11 * hkl::constant::math::degToRad,
                              41 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * hkl::constant::math::degToRad, g1.getDistance(g2), hkl::constant::math::epsilon);

  g2.get_axe("omega").set_current(10 * hkl::constant::math::degToRad);
  g2.get_axe("2theta").set_current(40 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.getDistance(g2), hkl::constant::math::epsilon);
}

void
GeometryTwoCTest::computeHKL(void)
{
  hkl::smatrix UB = _lattice.get_B();
  double h, k ,l;

  _geometry.get_source().setWaveLength(1.54);
  _geometry.setAngles(30 * hkl::constant::math::degToRad, 60 * hkl::constant::math::degToRad);
  _geometry.computeHKL(h, k, l, UB);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, h, hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, k, hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1, l, hkl::constant::math::epsilon);
}

void
GeometryTwoCTest::setFromGeometry(void)
{
  hkl::twoC::vertical::Geometry twoC;
  hkl::twoC::vertical::Geometry twoC_ref(10. * hkl::constant::math::degToRad,
                                    40. * hkl::constant::math::degToRad);

  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(10. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           40. * hkl::constant::math::degToRad);
  twoC.setFromGeometry(E4CV, true);
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(10. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        40. * hkl::constant::math::degToRad);
  twoC.setFromGeometry(K4CV, true);
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(0. * hkl::constant::math::degToRad,
                             10. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             40. * hkl::constant::math::degToRad);
  twoC.setFromGeometry(K6C, true);
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  // exceptions
  // eulerian4C
  E4CV.get_axe("chi").set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(E4CV, true), hkl::HKLException);
  // kappa4C
  K4CV.get_axe("kappa").set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K4CV, true), hkl::HKLException);
  // kappa6C
  K6C.get_axe("mu").set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), hkl::HKLException);
  K6C.get_axe("mu").set_current(0.);
  K6C.get_axe("gamma").set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), hkl::HKLException);
  K6C.get_axe("mu").set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), hkl::HKLException);
}

void
GeometryTwoCTest::persistanceIO(void)
{
  hkl::twoC::vertical::Geometry geometry1;
  hkl::twoC::vertical::Geometry geometry2;
  stringstream flux;

  _geometry.toStream(flux);
  _geometry.toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(_geometry, geometry2);
}
