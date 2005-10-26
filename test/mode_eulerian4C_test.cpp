// File to test quaternion implementation.
#include "mode_eulerian4C_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_Eulerian4C_Test );

void
Mode_Eulerian4C_Test::setUp(void)
{
  m_source.setWaveLength(1.54);
  
  m_crystal.setLattice(1.54, 1.54, 1.54, 90.*constant::math::degToRad, 90.*constant::math::degToRad, 90.*constant::math::degToRad);
  
  m_geometry_E4C.get_axe("2theta").set_value(60.*constant::math::degToRad);  
  m_geometry_E4C.get_axe("omega").set_value(30.*constant::math::degToRad);
  m_geometry_E4C.get_axe("chi").set_value(0.*constant::math::degToRad);
  m_geometry_E4C.get_axe("phi").set_value(90.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     1., 0., 0.,
                                     Reflection::Best, true));
  
  m_geometry_E4C.get_axe("phi").set_value(180.*constant::math::degToRad);
  m_crystal.addReflection(Reflection(m_geometry_E4C, m_source,
                                     0., 1., 0.,
                                     Reflection::Best, true));

  m_crystal.computeU();
}

void 
Mode_Eulerian4C_Test::tearDown(void) {}

void 
Mode_Eulerian4C_Test::Bissector(void)
{
  smatrix UB = m_crystal.get_U() * m_crystal.get_B();
  double const & lambda = m_source.get_waveLength();
  
  mode::eulerian4C::Bissector mode;
  
  mode.computeAngles(1., 0., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(-1., 0., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-90*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(0., 1., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(180.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(0.,-1., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
 
  mode.computeAngles(0., 0., 1., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  

  mode.computeAngles(0., 0., -1., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(30*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-90*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  

  mode.computeAngles(1., 1., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(45*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(135.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  
}

void 
Mode_Eulerian4C_Test::Delta_Theta(void)
{
  smatrix UB = m_crystal.get_U() * m_crystal.get_B();
  double const & lambda = m_source.get_waveLength();
  
  mode::eulerian4C::Delta_Theta mode;
  mode.setParameterValue("delta theta", 10 * constant::math::degToRad);
  
  mode.computeAngles(-1., 0., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-100*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
  
  mode.computeAngles(0., 1., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(170.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(0.,-1., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(60*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(40*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0.*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(-10.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
   
  mode.computeAngles(1., 1., 0., UB, lambda, m_geometry_E4C);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(90*constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(55*constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0*constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(125.*constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);  
 
  CPPUNIT_ASSERT_THROW( mode.computeAngles(0., 0., 1., UB, lambda, m_geometry_E4C), HKLException);
}
