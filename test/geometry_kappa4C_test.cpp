#include <iostream>
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"
#include "geometry_kappa4C_test.h"
#include "geometry_kappa6C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa4CTest );

void
GeometryKappa4CTest::setUp(void)
{
  m_alpha = 50 * constant::math::degToRad;
  m_geometry = new geometry::kappa4C::Vertical;
}

void
GeometryKappa4CTest::tearDown(void)
{
  delete m_geometry;
}

void
GeometryKappa4CTest::equal(void)
{
  geometry::kappa4C::Vertical geometry;
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa4CTest::copyConstructor(void)
{
  geometry::kappa4C::Vertical geometry(*m_geometry);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa4CTest::otherConstructors(void)
{
  double komega = 10 * constant::math::degToRad;
  double kappa = 11 * constant::math::degToRad;
  double kphi = 12 * constant::math::degToRad;
  double two_theta =13 * constant::math::degToRad;

  geometry::kappa4C::Vertical geometry_ref;
  geometry::kappa4C::Vertical geometry(komega, kappa, kphi, two_theta);

  geometry_ref.get_axe("komega").set_current(komega);
  geometry_ref.get_axe("kappa").set_current(kappa);
  geometry_ref.get_axe("kphi").set_current(kphi);
  geometry_ref.get_axe("2theta").set_current(two_theta);

  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa4CTest::get_alpha(void)
{
  CPPUNIT_ASSERT_EQUAL(m_alpha, m_geometry->get_alpha());
}

void
GeometryKappa4CTest::getAxesNames(void)
{
  vector<string> v = m_geometry->getAxesNames();
  CPPUNIT_ASSERT_EQUAL(string("komega"), v[0]);
  CPPUNIT_ASSERT_EQUAL(string("kappa"), v[1]);
  CPPUNIT_ASSERT_EQUAL(string("kphi"), v[2]);
  CPPUNIT_ASSERT_EQUAL(string("2theta"), v[3]);
}

void
GeometryKappa4CTest::getSampleQuaternion(void)
{
  m_geometry->get_axe("komega").set_current(90 * constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), m_geometry->getSampleQuaternion());
}

void
GeometryKappa4CTest::getSampleRotationMatrix(void)
{
  m_geometry->get_axe("komega").set_current(90. * constant::math::degToRad);

  smatrix M( 0., 0.,-1.,
             0., 1., 0.,
             1., 0., 0.);

  CPPUNIT_ASSERT_EQUAL(M, m_geometry->getSampleRotationMatrix());
}

void
GeometryKappa4CTest::getQ(void)
{
  m_geometry->get_axe("2theta").set_current(0. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry->getQ());

  m_geometry->get_source().setKi(svector(1, 0, 0));
  m_geometry->get_axe("2theta").set_current(45. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(1./sqrt(2)-1., 0, 1./sqrt(2.)), m_geometry->getQ());
}

void
GeometryKappa4CTest::getDistance(void)
{
  geometry::kappa4C::Vertical g1(10 * constant::math::degToRad,
                                 20 * constant::math::degToRad,
                                 30 * constant::math::degToRad,
                                 40 * constant::math::degToRad);

  geometry::kappa4C::Vertical g2(11 * constant::math::degToRad,
                                 21 * constant::math::degToRad,
                                 31 * constant::math::degToRad,
                                 41 * constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon);

  g2.get_axe("komega").set_current(10 * constant::math::degToRad);
  g2.get_axe("kappa").set_current(20 * constant::math::degToRad);
  g2.get_axe("kphi").set_current(30 * constant::math::degToRad);
  g2.get_axe("2theta").set_current(40 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon);
}

void
GeometryKappa4CTest::setAngles(void)
{
  double komega = 10 * constant::math::degToRad;
  double kappa = 20 * constant::math::degToRad;
  double kphi = 30 * constant::math::degToRad;
  double two_theta = 40 * constant::math::degToRad;

  m_geometry->setAngles(komega, kappa, kphi, two_theta);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(komega, m_geometry->get_axe("komega").get_current().get_value(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(kappa, m_geometry->get_axe("kappa").get_current().get_value(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(kphi, m_geometry->get_axe("kphi").get_current().get_value(), constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(two_theta, m_geometry->get_axe("2theta").get_current().get_value(), constant::math::epsilon);
}

void
GeometryKappa4CTest::setFromGeometry(void)
{
  geometry::kappa4C::Vertical K4CV;
  geometry::kappa4C::Vertical K4CV_ref(0. * constant::math::degToRad,
                                       0. * constant::math::degToRad,
                                       0. * constant::math::degToRad,
                                       40. * constant::math::degToRad);

  //eulerian4C::Vertical
  geometry::eulerian4C::Vertical E4CV(90. * constant::math::degToRad,
                                      0. * constant::math::degToRad,
                                      -90. * constant::math::degToRad,
                                      40. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E4CV, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  E4CV.get_axe("chi").set_current(110 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E4CV, true), HKLException);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E4CV, false), HKLException);

  //Eulerian6C
  geometry::Eulerian6C E6C(0. * constant::math::degToRad,
                           90. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           -90. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           40. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  E6C.get_axe("mu").set_current(1 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("gamma").set_current(1 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("mu").set_current(0 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  E6C.get_axe("chi").set_current(110 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, true), HKLException);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(E6C, false), HKLException);

  //Kappa6C
  geometry::Kappa6C K6C(0 * constant::math::degToRad,
                        0. * constant::math::degToRad,
                        0. * constant::math::degToRad,
                        0. * constant::math::degToRad,
                        0. * constant::math::degToRad,
                        40. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, true));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);

  K6C.get_axe("mu").set_current(1 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  K6C.get_axe("gamma").set_current(1 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);
  K6C.get_axe("mu").set_current(0 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K4CV.setFromGeometry(K6C, true), HKLException);
  CPPUNIT_ASSERT_NO_THROW(K4CV.setFromGeometry(K6C, false));
  CPPUNIT_ASSERT_EQUAL(K4CV_ref, K4CV);


}

void
GeometryKappa4CTest::persistanceIO(void)
{
  geometry::kappa4C::Vertical geometry1;
  geometry::kappa4C::Vertical geometry2;
  stringstream flux;

  m_geometry->get_axe("komega").set_current(2.);
  m_geometry->toStream(flux);
  m_geometry->get_axe("komega").set_current(3.);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  m_geometry->get_axe("komega").set_current(2.);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  m_geometry->get_axe("komega").set_current(3.);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
