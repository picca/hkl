#include "pseudoaxe_kappa6C_test.h"
#include "constants.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa6C_Test );

void
PseudoAxe_Kappa6C_Test::setUp(void)
{ 
    m_geometry_E4C = new geometry::eulerian4C::Vertical;

    m_alpha = 50. * hkl::constant::math::degToRad;
    m_geometry_K4C = new geometry::kappa4C::Vertical(m_alpha);
    m_geometry_K6C = new geometry::Kappa6C(m_alpha);
}

void 
PseudoAxe_Kappa6C_Test::tearDown(void)
{
    delete m_geometry_K6C;
    delete m_geometry_K4C;
    delete m_geometry_E4C;
}

void 
PseudoAxe_Kappa6C_Test::Omega(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa6C::kappa4C::vertical::Omega omega(m_alpha);

    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        omega.set_value(*m_geometry_K6C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, omega.get_value(*m_geometry_K6C), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa6C_Test::Chi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa6C::kappa4C::vertical::Chi pseudo(m_alpha);
    int chi_max = 2 * (int)(m_alpha * hkl::constant::math::radToDeg);

    //test exception if chi > 2*alpha
    angle = chi_max + 0.1;
    CPPUNIT_ASSERT_THROW(pseudo.set_value(*m_geometry_K6C, angle), HKLException);

    for(i=-chi_max;i<chi_max;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry_K6C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry_K6C), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa6C_Test::Phi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa6C::kappa4C::vertical::Phi pseudo(m_alpha);

    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry_K6C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry_K6C), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa6C_Test::Psi(void)
{
    int i;
    double angle = 10. * hkl::constant::math::degToRad;
    hkl::pseudoAxe::kappa6C::kappa4C::vertical::Psi psi(m_alpha);

    m_geometry_E4C->setAngles(45. * constant::math::degToRad,
                              77. * constant::math::degToRad,
                              -5. * constant::math::degToRad,
                              34. * constant::math::degToRad);  
    m_geometry_K4C->setFromGeometry(*m_geometry_E4C, true);
    m_geometry_K6C->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry_K6C);

    //set_value test1 non degenerate case
    psi.set_value(*m_geometry_K6C, 0. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry_K6C, true);
    m_geometry_E4C->setFromGeometry(*m_geometry_K4C, true);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45. * constant::math::degToRad, m_geometry_E4C->get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77. * constant::math::degToRad, m_geometry_E4C->get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5. * constant::math::degToRad, m_geometry_E4C->get_axe("phi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, m_geometry_E4C->get_axe("2theta").get_value(), constant::math::epsilon_0);

    //set_value test2 degenerate case
    m_geometry_E4C->setAngles(30. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              60. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry_E4C, true);
    m_geometry_K6C->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry_K6C);
    psi.set_value(*m_geometry_K6C, 0. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry_K6C, true);
    m_geometry_E4C->setFromGeometry(*m_geometry_K4C, true);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30. * constant::math::degToRad,
                                 m_geometry_E4C->get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad,
                                 m_geometry_E4C->get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad,
                                 m_geometry_E4C->get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60. * constant::math::degToRad,
                                 m_geometry_E4C->get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    //get_value test
    m_geometry_E4C->setAngles(45. * constant::math::degToRad,
                              77. * constant::math::degToRad,
                              180. * constant::math::degToRad,
                              34. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry_E4C, true);
    m_geometry_K6C->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry_K6C);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if ((i <= -174) || (i >= 47))
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(*m_geometry_K6C, angle), HKLException);
          }
        else
          {
            psi.set_value(*m_geometry_K6C, angle);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(*m_geometry_K6C), constant::math::epsilon_0);
          }
      }

    m_geometry_E4C->setAngles(30. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              60. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry_E4C, true);
    m_geometry_K6C->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry_K6C);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if (abs(i) > 100)
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(*m_geometry_K6C, angle), HKLException);
          }
        else
          {
            psi.set_value(*m_geometry_K6C, angle);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(*m_geometry_K6C), constant::math::epsilon_0);
          }
      }
}

void
PseudoAxe_Kappa6C_Test::persistanceIO(void)
{
    hkl::pseudoAxe::kappa6C::kappa4C::vertical::Omega omega_ref(m_alpha);
    hkl::pseudoAxe::kappa6C::kappa4C::vertical::Omega omega(1.);
    stringstream flux;

    omega_ref.toStream(flux);

    omega.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(omega_ref, omega);
}
