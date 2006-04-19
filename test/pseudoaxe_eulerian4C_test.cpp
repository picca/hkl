// File to test quaternion implementation.
#include "pseudoaxe_eulerian4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Vertical_Test );

void
PseudoAxe_Eulerian4C_Vertical_Test::setUp(void)
{ 
    m_geometry_E4C.get_source().setWaveLength(1.54);

    m_geometry_E4C.get_axe("omega").set_value(45. * constant::math::degToRad);
    m_geometry_E4C.get_axe("chi").set_value(77. * constant::math::degToRad);
    m_geometry_E4C.get_axe("phi").set_value(-5. * constant::math::degToRad);
    m_geometry_E4C.get_axe("2theta").set_value(34. * constant::math::degToRad);  
}

void 
PseudoAxe_Eulerian4C_Vertical_Test::tearDown(void)
{}

void 
PseudoAxe_Eulerian4C_Vertical_Test::Psi(void)
{
    int i;
    double angle = 10. * hkl::constant::math::degToRad;
    hkl::pseudoAxe::eulerian4C::vertical::Psi psi;

    psi.initialize(m_geometry_E4C);

    //set_value test1 non degenerate case
    psi.set_value(m_geometry_E4C, 0. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    //set_value test2 degenerate case
    m_geometry_E4C = hkl::geometry::eulerian4C::Vertical(30 * constant::math::degToRad,
                                                         0 * constant::math::degToRad,
                                                         0 * constant::math::degToRad,
                                                         60 * constant::math::degToRad);
    psi.initialize(m_geometry_E4C);
    psi.set_value(m_geometry_E4C, 0. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60 * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    //get_value test
    m_geometry_E4C = hkl::geometry::eulerian4C::Vertical(45 * constant::math::degToRad,
                                                         77 * constant::math::degToRad,
                                                         180 * constant::math::degToRad,
                                                         34 * constant::math::degToRad);

    psi.initialize(m_geometry_E4C);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        psi.set_value(m_geometry_E4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(m_geometry_E4C), constant::math::epsilon_0);
      }

    m_geometry_E4C = hkl::geometry::eulerian4C::Vertical(30 * constant::math::degToRad,
                                                         0 * constant::math::degToRad,
                                                         0 * constant::math::degToRad,
                                                         60 * constant::math::degToRad);

    psi.initialize(m_geometry_E4C);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        psi.set_value(m_geometry_E4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle,
                                     psi.get_value(m_geometry_E4C),
                                     constant::math::epsilon_0);
      }
}

void
PseudoAxe_Eulerian4C_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::eulerian4C::vertical::Psi psi_ref, psi;
    stringstream flux;

    psi_ref.toStream(flux);

    psi.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
}
