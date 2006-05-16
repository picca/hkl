#include "mode_twoC_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_TwoC_Test );

void
Mode_TwoC_Test::setUp(void)
{
  m_geometry.get_source().setWaveLength(1.54);
  
  m_crystal.setLattice(1.54, 1.54, 1.54, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad);
  
  m_geometry.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry.get_axe("2theta").set_value(60.*constant::math::degToRad);  
  m_crystal.addReflection(Reflection(m_geometry,
                                     0., 0., 1.,
                                     Reflection::Best, true));
  
  m_geometry.get_axe("omega").set_value(120.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry,
                                     0., 1., 0.,
                                     Reflection::Best, true));

  m_crystal.computeU();
}

void 
Mode_TwoC_Test::tearDown(void) {}

void 
Mode_TwoC_Test::Symetric(void)
{
  smatrix UB = m_crystal.get_U() * m_crystal.get_B();
  
  mode::twoC::vertical::Symetric mode;
  
  mode.computeAngles(1., 0., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(-1., 0., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(0., 1., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(0.,-1., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
 
  mode.computeAngles(0., 0., 1., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);

  mode.computeAngles(0., 0., -1., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);

  mode.computeAngles(1., 1., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(45*constant::math::degToRad, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
}

void 
Mode_TwoC_Test::Fix_Incidence(void)
{
  smatrix UB = m_crystal.get_U() * m_crystal.get_B();
  
  mode::twoC::vertical::Fix_Incidence mode;
 
  // omega must not change in this mode.
  double omega = m_geometry.get_axe("omega").get_value();
  
  mode.computeAngles(-1., 0., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(0., 1., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(0.,-1., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(1., 1., 0., UB, m_geometry);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(omega, m_geometry.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry.get_axe("2theta").get_value(), constant::math::epsilon_0);
}

void
Mode_TwoC_Test::persistanceIO(void)
{
  mode::twoC::vertical::Symetric symetric_ref, symetric;
  mode::twoC::vertical::Fix_Incidence fix_incidence_ref, fix_incidence;
  stringstream flux;
  
  symetric_ref.toStream(flux);
  fix_incidence_ref.toStream(flux);
  symetric.fromStream(flux);
  fix_incidence.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(symetric_ref, symetric);
  CPPUNIT_ASSERT_EQUAL(fix_incidence_ref, fix_incidence);
}
