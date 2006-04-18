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

    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        omega.set_value(*m_geometry_K4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, omega.get_value(*m_geometry_K4C), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Test::Chi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::Chi pseudo(m_alpha);
    int chi_max = 2 * (int)(m_alpha * hkl::constant::math::radToDeg);

    //test exception if chi > 2*alpha
    angle = chi_max + 0.1;
    CPPUNIT_ASSERT_THROW(pseudo.set_value(*m_geometry_K4C, angle), HKLException);

    for(i=-chi_max;i<chi_max;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry_K4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry_K4C), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Test::Phi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::Phi pseudo(m_alpha);

    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry_K4C, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry_K4C), constant::math::epsilon_0);
      }
}

void
PseudoAxe_Kappa4C_Test::persistanceIO(void)
{
    hkl::pseudoAxe::kappa4C::Omega omega_ref(m_alpha);
    hkl::pseudoAxe::kappa4C::Omega omega(1.);
    stringstream flux;

    omega_ref.toStream(flux);

    omega.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(omega_ref, omega);
}
