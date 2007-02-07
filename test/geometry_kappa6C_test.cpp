#include <sstream>
#include "geometry_kappa6C_test.h"
#include "geometry_eulerian4C.h"
#include "geometry_eulerian6C.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa6CTest );

void
GeometryKappa6CTest::setUp(void)
{
  m_alpha = 50 * constant::math::degToRad;
  m_geometry = new geometry::Kappa6C;
}

void
GeometryKappa6CTest::tearDown(void)
{
  delete m_geometry;
}

void
GeometryKappa6CTest::equal(void)
{
  geometry::Kappa6C geometry;
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa6CTest::copyConstructor(void)
{
  geometry::Kappa6C geometry(*m_geometry);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa6CTest::otherConstructors(void)
{
  double mu = 9 * constant::math::degToRad;
  double komega = 10 * constant::math::degToRad;
  double kappa = 11 * constant::math::degToRad;
  double kphi = 12 * constant::math::degToRad;

  double gamma =13 * constant::math::degToRad;
  double delta =14 * constant::math::degToRad;

  geometry::Kappa6C geometry_ref;
  geometry::Kappa6C geometry(mu, komega, kappa, kphi, gamma, delta);

  geometry_ref.get_axe("mu").set_current(mu);
  geometry_ref.get_axe("komega").set_current(komega);
  geometry_ref.get_axe("kappa").set_current(kappa);
  geometry_ref.get_axe("kphi").set_current(kphi);
  geometry_ref.get_axe("gamma").set_current(gamma);
  geometry_ref.get_axe("delta").set_current(delta);

  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa6CTest::getAxesNames(void)
{
  vector<string> v = m_geometry->getAxesNames();
  CPPUNIT_ASSERT_EQUAL(string("mu"), v[0]);
  CPPUNIT_ASSERT_EQUAL(string("komega"), v[1]);
  CPPUNIT_ASSERT_EQUAL(string("kappa"), v[2]);
  CPPUNIT_ASSERT_EQUAL(string("kphi"), v[3]);
  CPPUNIT_ASSERT_EQUAL(string("gamma"), v[4]);
  CPPUNIT_ASSERT_EQUAL(string("delta"), v[5]);
}

void
GeometryKappa6CTest::getSampleQuaternion(void)
{
  m_geometry->get_axe("mu").set_current(90 * constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry->getSampleQuaternion());
}

void
GeometryKappa6CTest::getSampleRotationMatrix(void)
{
  m_geometry->get_axe("mu").set_current(90. * constant::math::degToRad);

  smatrix M( 0.,-1., 0.,
             1., 0., 0.,
             0., 0., 1.);

  CPPUNIT_ASSERT_EQUAL(M, m_geometry->getSampleRotationMatrix());
}

void
GeometryKappa6CTest::getQ(void)
{
  m_geometry->get_axe("gamma").set_current(0. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry->getQ());

  m_geometry->get_source().setKi(svector(1, 0, 0));
  m_geometry->get_axe("gamma").set_current(45. * constant::math::degToRad);
  m_geometry->get_axe("delta").set_current(45. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(-.5, .5, sqrt(2.)/2.), m_geometry->getQ());
}

void
GeometryKappa6CTest::getDistance(void)
{
  geometry::Kappa6C g1(10 * constant::math::degToRad,
                       20 * constant::math::degToRad,
                       30 * constant::math::degToRad,
                       40 * constant::math::degToRad,
                       50 * constant::math::degToRad,
                       60 * constant::math::degToRad);

  geometry::Kappa6C g2(11 * constant::math::degToRad,
                       21 * constant::math::degToRad,
                       31 * constant::math::degToRad,
                       41 * constant::math::degToRad,
                       51 * constant::math::degToRad,
                       61 * constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon);

  g2.get_axe("mu").set_current(10 * constant::math::degToRad);
  g2.get_axe("komega").set_current(20 * constant::math::degToRad);
  g2.get_axe("kappa").set_current(30 * constant::math::degToRad);
  g2.get_axe("kphi").set_current(40 * constant::math::degToRad);
  g2.get_axe("gamma").set_current(50 * constant::math::degToRad);
  g2.get_axe("delta").set_current(60 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon);
}

void
GeometryKappa6CTest::setFromGeometry(void)
{
  double alpha = 50. * constant::math::degToRad;
  geometry::Kappa6C K6C;
  geometry::Kappa6C K6C_ref(0. * constant::math::degToRad,
                            0. * constant::math::degToRad,
                            0. * constant::math::degToRad,
                            0. * constant::math::degToRad,
                            0. * constant::math::degToRad,
                            40. * constant::math::degToRad);

  //eulerian4C::Vertical
  geometry::eulerian4C::Vertical E4CV(90. * constant::math::degToRad,
                                      0. * constant::math::degToRad,
                                      -90. * constant::math::degToRad,
                                      40. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E4CV, false));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);

  E4CV.get_axe("chi").set_current(110 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E4CV, true), HKLException);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E4CV, false), HKLException);

  //Eulerian6C
  geometry::Eulerian6C E6C(0. * constant::math::degToRad,
                           90. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           -90. * constant::math::degToRad,
                           0. * constant::math::degToRad,
                           40. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E6C, true));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);

  E6C.get_axe("chi").set_current(110 * constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E6C, true), HKLException);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E6C, false), HKLException);
}

void
GeometryKappa6CTest::persistanceIO(void)
{
  geometry::Kappa6C geometry1;
  geometry::Kappa6C geometry2;
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
