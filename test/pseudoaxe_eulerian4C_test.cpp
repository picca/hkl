// File to test quaternion implementation.
#include "pseudoaxe_eulerian4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Test );

void
PseudoAxe_Eulerian4C_Test::setUp(void)
{ 
  m_geometry_E4C.get_source().setWaveLength(1.54);
  
  m_geometry_E4C.get_axe("2theta").set_value(34.*hkl::constant::math::degToRad);  
  m_geometry_E4C.get_axe("omega").set_value(45.*hkl::constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(77.*hkl::constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(-5.*hkl::constant::math::degToRad);
}

void 
PseudoAxe_Eulerian4C_Test::tearDown(void)
{}

void 
PseudoAxe_Eulerian4C_Test::Psi(void)
{
  hkl::pseudoAxe::eulerian4C::Psi psi;
  
  psi.init(m_geometry_E4C);
  
  psi.set_value(m_geometry_E4C, 10. * hkl::constant::math::degToRad);
  
  //psi.get_value(m_geometry_E4C);

  /*
  mode.computeAngles(1., 0., 0., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(-1., 0., 0., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-90*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(0., 1., 0., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(0.,-1., 0., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
 
  mode.computeAngles(0., 0., 1., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  

  mode.computeAngles(0., 0., -1., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-90*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  

  mode.computeAngles(1., 1., 0., UB, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(45*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(135.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  
  */
}
