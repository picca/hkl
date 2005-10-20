// File to test angleconfiguration implementation.
#include "angleconfiguration_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( angleConfigurationTest );

void
angleConfigurationTest::setUp()
{}

void 
angleConfigurationTest::tearDown() 
{}

void
angleConfigurationTest::Equal()
{
  CPPUNIT_ASSERT_EQUAL(m_aC_E4C, m_aC_E4C);
  CPPUNIT_ASSERT_EQUAL(m_aC_E6C, m_aC_E6C);
}

void 
angleConfigurationTest::CopyConstructor()
{
  angleConfiguration::Eulerian4C aC_E4C(m_aC_E4C);
  angleConfiguration::Eulerian6C aC_E6C(m_aC_E6C);

  CPPUNIT_ASSERT_EQUAL(m_aC_E4C, aC_E4C);
  CPPUNIT_ASSERT_EQUAL(m_aC_E6C, aC_E6C);
}

void
angleConfigurationTest::GetAxesNames()
{
  std::vector<std::string> v = m_aC_E4C.getAxesNames();
  CPPUNIT_ASSERT_EQUAL(v[0], std::string("omega"));
  CPPUNIT_ASSERT_EQUAL(v[1], std::string("chi"));
  CPPUNIT_ASSERT_EQUAL(v[2], std::string("phi"));
  CPPUNIT_ASSERT_EQUAL(v[3], std::string("2theta"));
}

void
angleConfigurationTest::AddSampleDetectorAxe()
{
  AngleConfiguration aC;
  
  Axe A("a", svector(0., 0., 1.), 1);
  Axe B("a", svector(0., 0., 1.), -1);

  // On peut ajouter un Axe dans la partie sample
  CPPUNIT_ASSERT_NO_THROW(aC.addSampleAxe(A));
  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(aC.addSampleAxe(A), HKLException);
  // Par contre on peut ajouter le même axe dans la partie detecteur.
  CPPUNIT_ASSERT_NO_THROW(aC.addDetectorAxe(A));
  // et encore une fois pas une deuxieme fois.
  CPPUNIT_ASSERT_THROW(aC.addDetectorAxe(A), HKLException);
  
  // On verifie que l'on ne peut pas mettre dans la liste m_axeMap
  // deux axes differents mais ayant le même nom.
  CPPUNIT_ASSERT_THROW(aC.addSampleAxe(B), HKLException);
  CPPUNIT_ASSERT_THROW(aC.addDetectorAxe(B), HKLException);
}

void
angleConfigurationTest::Operateurs()
{
  Axe Nu("nu", svector(0., 0., 1.), 1);
  Axe Omega("omega", svector(0., 1., 0.), -1);
  Axe Chi("chi", svector(1., 0., 0.), 1);
  Axe Phi("phi", svector(0., 1., 0.), -1);
  Axe Gamma("gamma", svector(0., 0., 1.), 1);
  Axe Delta("delta", svector(0., 1., 0.), -1);

  // on verifie que les exceptions sont bien lancees lorsque
  // l'on recherche un axe qui n'existe pas.
  CPPUNIT_ASSERT_THROW(m_aC_E6C["toto"], HKLException);
  
  // et que tout se passe bien le cas contraire.
  CPPUNIT_ASSERT_NO_THROW(m_aC_E6C["omega"]);
  CPPUNIT_ASSERT_NO_THROW(m_aC_E6C["gamma"]);
  
  // On verifie que les valeurs retournées sont les bonnes.
  CPPUNIT_ASSERT_EQUAL(Nu, m_aC_E6C["nu"]);
  CPPUNIT_ASSERT_EQUAL(Omega, m_aC_E6C["omega"]);
  CPPUNIT_ASSERT_EQUAL(Chi, m_aC_E6C["chi"]);
  CPPUNIT_ASSERT_EQUAL(Phi, m_aC_E6C["phi"]);
  CPPUNIT_ASSERT_EQUAL(Gamma, m_aC_E6C["gamma"]);
  CPPUNIT_ASSERT_EQUAL(Delta, m_aC_E6C["delta"]);
}

void
angleConfigurationTest::GetSampleRotationMatrix()
{
  m_aC_E6C["nu"].set_value(90. * constant::math::degToRad);

  smatrix M( 0.,-1., 0.,
             1., 0., 0.,
             0., 0., 1.);

  CPPUNIT_ASSERT_EQUAL(M, m_aC_E6C.getSampleRotationMatrix());
}

void
angleConfigurationTest::GetQ()
{
  m_aC_E6C["gamma"].set_value(0. * constant::math::degToRad);
  m_aC_E6C["delta"].set_value(0. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(0., 0., 0.), m_aC_E6C.getQ(Quaternion()));
  
  m_aC_E6C["gamma"].set_value(45. * constant::math::degToRad);
  m_aC_E6C["delta"].set_value(45. * constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(svector(-.5, .5, sqrt(2.)/2.), m_aC_E6C.getQ(Quaternion(0., 1., 0., 0.)));
}
