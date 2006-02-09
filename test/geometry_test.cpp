// File to test angleconfiguration implementation.
#include "geometry_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTest );

void
GeometryTest::setUp(void) {}

void 
GeometryTest::tearDown(void) {}

void
GeometryTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(m_geometry_E4C, m_geometry_E4C);
  CPPUNIT_ASSERT_EQUAL(m_geometry_E6C, m_geometry_E6C);
}

void 
GeometryTest::copyConstructor(void)
{
  geometry::Eulerian4C aC_E4C(m_geometry_E4C);
  geometry::Eulerian6C aC_E6C(m_geometry_E6C);

  CPPUNIT_ASSERT_EQUAL(m_geometry_E4C, aC_E4C);
  CPPUNIT_ASSERT_EQUAL(m_geometry_E6C, aC_E6C);
}

void 
GeometryTest::otherConstructors(void)
{
  double omega = 10 * constant::math::degToRad;
  double chi = 11 * constant::math::degToRad;
  double phi = 12 * constant::math::degToRad;
  double two_theta =13 * constant::math::degToRad;
 
  geometry::Eulerian4C geometry_E4C_ref;
  geometry::Eulerian4C geometry_E4C(omega, chi, phi, two_theta);

  geometry_E4C_ref.get_axe("omega").set_value(omega);
  geometry_E4C_ref.get_axe("chi").set_value(chi);
  geometry_E4C_ref.get_axe("phi").set_value(phi);
  geometry_E4C_ref.get_axe("2theta").set_value(two_theta);

  CPPUNIT_ASSERT_EQUAL(geometry_E4C_ref, geometry_E4C);
}
void
GeometryTest::getAxesNames(void)
{
  vector<MyString> v = m_geometry_E4C.getAxesNames();
  CPPUNIT_ASSERT_EQUAL(MyString("omega"), v[0]);
  CPPUNIT_ASSERT_EQUAL(MyString("chi"), v[1]);
  CPPUNIT_ASSERT_EQUAL(MyString("phi"), v[2]);
  CPPUNIT_ASSERT_EQUAL(MyString("2theta"), v[3]);
}

void
GeometryTest::addSampleDetectorAxe(void)
{
  Geometry geometry;
  
  Axe A("a", svector(0., 0., 1.), 1);
  Axe B("a", svector(0., 0., 1.), -1);

  // On peut ajouter un Axe dans la partie sample
  CPPUNIT_ASSERT_NO_THROW(geometry.addSampleAxe(A));
  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(geometry.addSampleAxe(A), HKLException);
  // Par contre on peut ajouter le même axe dans la partie detecteur.
  CPPUNIT_ASSERT_NO_THROW(geometry.addDetectorAxe(A));
  // et encore une fois pas une deuxieme fois.
  CPPUNIT_ASSERT_THROW(geometry.addDetectorAxe(A), HKLException);
  
  // On verifie que l'on ne peut pas mettre dans la liste m_axeMap
  // deux axes differents mais ayant le même nom.
  CPPUNIT_ASSERT_THROW(geometry.addSampleAxe(B), HKLException);
  CPPUNIT_ASSERT_THROW(geometry.addDetectorAxe(B), HKLException);
}

void
GeometryTest::operateurs(void)
{
  Axe Nu("nu", svector(0., 0., 1.), 1);
  Axe Omega("omega", svector(0., 1., 0.), -1);
  Axe Chi("chi", svector(1., 0., 0.), 1);
  Axe Phi("phi", svector(0., 1., 0.), -1);
  Axe Gamma("gamma", svector(0., 0., 1.), 1);
  Axe Delta("delta", svector(0., 1., 0.), -1);

  // on verifie que les exceptions sont bien lancees lorsque
  // l'on recherche un axe qui n'existe pas.
  CPPUNIT_ASSERT_THROW(m_geometry_E6C.get_axe("toto"), HKLException);
  
  // et que tout se passe bien le cas contraire.
  CPPUNIT_ASSERT_NO_THROW(m_geometry_E6C.get_axe("omega"));
  CPPUNIT_ASSERT_NO_THROW(m_geometry_E6C.get_axe("gamma"));
  
  // On verifie que les valeurs retournées sont les bonnes.
  CPPUNIT_ASSERT_EQUAL(Nu, m_geometry_E6C.get_axe("nu"));
  CPPUNIT_ASSERT_EQUAL(Omega, m_geometry_E6C.get_axe("omega"));
  CPPUNIT_ASSERT_EQUAL(Chi, m_geometry_E6C.get_axe("chi"));
  CPPUNIT_ASSERT_EQUAL(Phi, m_geometry_E6C.get_axe("phi"));
  CPPUNIT_ASSERT_EQUAL(Gamma, m_geometry_E6C.get_axe("gamma"));
  CPPUNIT_ASSERT_EQUAL(Delta, m_geometry_E6C.get_axe("delta"));
}

void
GeometryTest::getSampleQuaternion(void)
{
  m_geometry_E6C.get_axe("nu").set_value(90 * constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry_E6C.getSampleQuaternion());
}

void
GeometryTest::getSampleRotationMatrix(void)
{
  m_geometry_E6C.get_axe("nu").set_value(90. * constant::math::degToRad);

  smatrix M( 0.,-1., 0.,
             1., 0., 0.,
             0., 0., 1.);

  CPPUNIT_ASSERT_EQUAL(M, m_geometry_E6C.getSampleRotationMatrix());
}

void
GeometryTest::getQ(void)
{
  m_geometry_E6C.get_axe("gamma").set_value(0. * constant::math::degToRad);
  m_geometry_E6C.get_axe("delta").set_value(0. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry_E6C.getQ());
  
  m_geometry_E6C.get_axe("gamma").set_value(45. * constant::math::degToRad);
  m_geometry_E6C.get_axe("delta").set_value(45. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(-.5, .5, sqrt(2.)/2.), m_geometry_E6C.getQ());
}

void
GeometryTest::getDistance(void)
{
  geometry::Eulerian4C g1(10 * constant::math::degToRad,
                          20 * constant::math::degToRad,
                          30 * constant::math::degToRad,
                          40 * constant::math::degToRad);
  
  geometry::Eulerian4C g2(11 * constant::math::degToRad,
                          21 * constant::math::degToRad,
                          31 * constant::math::degToRad,
                          41 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(4. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);
  
  g2.get_axe("omega").set_value(10 * constant::math::degToRad);
  g2.get_axe("chi").set_value(20 * constant::math::degToRad);
  g2.get_axe("phi").set_value(30 * constant::math::degToRad);
  g2.get_axe("2theta").set_value(40 * constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad, g1.getDistance(g2), constant::math::epsilon_0);
}

void
GeometryTest::persistanceIO(void)
{
  geometry::Eulerian6C geometry_E6C;
  geometry::Eulerian4C geometry_E4C;  
  stringstream flux;
  
  m_geometry_E6C.toStream(flux);
  m_geometry_E4C.toStream(flux);  
  geometry_E6C.fromStream(flux);
  geometry_E4C.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(m_geometry_E6C, geometry_E6C);
  CPPUNIT_ASSERT_EQUAL(m_geometry_E4C, geometry_E4C);
}
