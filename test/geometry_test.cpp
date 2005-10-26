// File to test angleconfiguration implementation.
#include "geometry_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTest );

void
GeometryTest::setUp(void) {}

void 
GeometryTest::tearDown(void) {}

void
GeometryTest::Equal(void)
{
  CPPUNIT_ASSERT_EQUAL(m_geometry_E4C, m_geometry_E4C);
  CPPUNIT_ASSERT_EQUAL(m_geometry_E6C, m_geometry_E6C);
}

void 
GeometryTest::CopyConstructor(void)
{
  geometry::Eulerian4C aC_E4C(m_geometry_E4C);
  geometry::Eulerian6C aC_E6C(m_geometry_E6C);

  CPPUNIT_ASSERT_EQUAL(m_geometry_E4C, aC_E4C);
  CPPUNIT_ASSERT_EQUAL(m_geometry_E6C, aC_E6C);
}

void
GeometryTest::GetAxesNames(void)
{
  std::vector<std::string> v = m_geometry_E4C.getAxesNames();
  CPPUNIT_ASSERT_EQUAL(std::string("omega"), v[0]);
  CPPUNIT_ASSERT_EQUAL(std::string("chi"), v[1]);
  CPPUNIT_ASSERT_EQUAL(std::string("phi"), v[2]);
  CPPUNIT_ASSERT_EQUAL(std::string("2theta"), v[3]);
}

void
GeometryTest::AddSampleDetectorAxe(void)
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
GeometryTest::Operateurs(void)
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
GeometryTest::GetSampleRotationMatrix(void)
{
  m_geometry_E6C.get_axe("nu").set_value(90. * constant::math::degToRad);

  smatrix M( 0.,-1., 0.,
             1., 0., 0.,
             0., 0., 1.);

  CPPUNIT_ASSERT_EQUAL(M, m_geometry_E6C.getSampleRotationMatrix());
}

void
GeometryTest::GetQ(void)
{
  m_geometry_E6C.get_axe("gamma").set_value(0. * constant::math::degToRad);
  m_geometry_E6C.get_axe("delta").set_value(0. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_geometry_E6C.getQ(Quaternion()));
  
  m_geometry_E6C.get_axe("gamma").set_value(45. * constant::math::degToRad);
  m_geometry_E6C.get_axe("delta").set_value(45. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(-.5, .5, sqrt(2.)/2.), m_geometry_E6C.getQ(Quaternion(0., 1., 0., 0.)));
}
