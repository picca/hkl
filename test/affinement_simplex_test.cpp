#include "affinement_simplex_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Affinement_SimplexTest );

void
Affinement_SimplexTest::setUp(void)
{
  // initialisation de la source
  m_geometry_E4C.get_source().setWaveLength(1.54);

  m_crystal.setLattice(1., 5., 4., 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90. * constant::math::degToRad);
 
  // Reflection 1
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(90.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, 1., 0., 0., 0, true));
  
  // Reflection 2
  m_geometry_E4C.get_axe("2theta").set_value(60*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(90.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, 0., 1., 0., 0, true));

  // Reflection 3
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, 0., 0., 1., 0, true));

  // Reflection 4
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(60.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, 0.625, 0.75, -0.216506350946, 0, true));
  
  // Reflection 5
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(45.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(45.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(45.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, 0.665975615037, 0.683012701892, 0.299950211252, 0, true));
}

void 
Affinement_SimplexTest::tearDown(void) {}

void
Affinement_SimplexTest::Fit(void)
{
  double a, b, c, alpha, beta, gamma;
  smatrix U(1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.);
  
  m_simplex.set_nb_max_iteration(10000);
  m_simplex.fit(m_crystal);

  m_crystal.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.54, a, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.54, b, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1.54, c, constant::math::epsilon_1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.*constant::math::degToRad, alpha, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.*constant::math::degToRad, beta, constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90.*constant::math::degToRad, gamma, constant::math::epsilon_0);
  CPPUNIT_ASSERT_EQUAL(U, m_crystal.get_U());
}

void
Affinement_SimplexTest::Fit2(void)
{
  Crystal crystal;
  
  // initialisation de la source
  m_geometry_E4C.get_source().setWaveLength(1.54);

  crystal.setLattice(4.81, 8.47, 2.941, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90. * constant::math::degToRad);

  crystal["a"].set_flagFit(false);
  crystal["b"].set_flagFit(false);
  crystal["c"].set_flagFit(false);
  //crystal["alpha"].set_flagFit(false);
  //crystal["beta"].set_flagFit(false);
  //crystal["gamma"].set_flagFit(false);
  
  // Reflection 1
  m_geometry_E4C.get_axe("2theta").set_value(30.45*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(12.4*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(88.3*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(0.*constant::math::degToRad);
  crystal.addReflection(Reflection(m_geometry_E4C, 0., 0., 1., 0, true));
  
  // Reflection 2
  m_geometry_E4C.get_axe("2theta").set_value(21.*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(10.95*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(-1.6*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(-2.*constant::math::degToRad);
  crystal.addReflection(Reflection(m_geometry_E4C, 0., 2., 0., 0, true));

  // Reflection 3
  m_geometry_E4C.get_axe("2theta").set_value(53.85*constant::math::degToRad);
  m_geometry_E4C.get_axe("omega").set_value(27.339*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(34.17*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(56.8*constant::math::degToRad);
  crystal.addReflection(Reflection(m_geometry_E4C, -2, 2., 1., 0, true));

  m_simplex.set_nb_max_iteration(10000);
  Crystal crystal1(crystal);
  m_simplex.fit(crystal);
  std::cout << crystal1;
  std::cout << crystal;
}
