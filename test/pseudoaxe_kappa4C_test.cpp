#include "pseudoaxe_kappa4C_test.h"
#include "constants.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa4C_Test );

void
PseudoAxe_Kappa4C_Test::setUp(void)
{ 
    m_alpha = 50. * hkl::constant::math::degToRad;
    m_geometry_K4C = new geometry::Kappa4C(m_alpha);
    
    m_geometry_K4C->get_source().setWaveLength(1.54);

    m_geometry_K4C->get_axe("komega").set_value(30. * constant::math::degToRad);
    m_geometry_K4C->get_axe("kappa").set_value(90. * constant::math::degToRad);
    m_geometry_K4C->get_axe("kphi").set_value(0. * constant::math::degToRad);
    m_geometry_K4C->get_axe("2theta").set_value(60. * constant::math::degToRad);  
}

void 
PseudoAxe_Kappa4C_Test::tearDown(void)
{
  delete m_geometry_K4C;
}

void 
PseudoAxe_Kappa4C_Test::Omega(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::Omega omega(m_alpha);

    //psi.initialize(*m_geometry_K4C);

    //cout << "omega" << endl;
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        omega.set_value(*m_geometry_K4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, omega.get_value(*m_geometry_K4C), constant::math::epsilon_0);
        //cout << i << " " << omega.get_value(*m_geometry_K4C) * hkl::constant::math::radToDeg << endl;
      }

    //set_value test1 non degenerate case
    //omega.set_value(*m_geometry_K4C, 5. * constant::math::degToRad);
    //cout << omega.get_value(*m_geometry_K4C) * hkl::constant::math::radToDeg << endl;
    /*
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry_K4C->get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry_K4C->get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry_K4C->get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60. * constant::math::degToRad,
                                 m_geometry_K4C->get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    //set_value test2 degenerate case
    m_geometry_E4C = hkl::geometry::Eulerian4C(30 * constant::math::degToRad,
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
    m_geometry_E4C = hkl::geometry::Eulerian4C(45 * constant::math::degToRad,
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

    m_geometry_E4C = hkl::geometry::Eulerian4C(30 * constant::math::degToRad,
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
      */
}

void 
PseudoAxe_Kappa4C_Test::Chi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::Chi pseudo(m_alpha);

    //psi.initialize(*m_geometry_K4C);

    //cout << "chi" << endl;
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry_K4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry_K4C), constant::math::epsilon_0);
        //cout << i << " " << pseudo.get_value(*m_geometry_K4C) * hkl::constant::math::radToDeg << endl;
      }
}

void 
PseudoAxe_Kappa4C_Test::Phi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::Phi pseudo(m_alpha);

    //psi.initialize(*m_geometry_K4C);

    //cout << "phi" << endl;
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry_K4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry_K4C), constant::math::epsilon_0);
        //cout << i << " " << pseudo.get_value(*m_geometry_K4C) * hkl::constant::math::radToDeg << endl;
      }
}

void
PseudoAxe_Kappa4C_Test::persistanceIO(void)
{
    hkl::pseudoAxe::kappa4C::Omega omega_ref(m_alpha), omega(m_alpha);
    stringstream flux;

    omega_ref.toStream(flux);

    omega.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(omega_ref, omega);
}
