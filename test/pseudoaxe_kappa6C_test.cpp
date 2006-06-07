#include "pseudoaxe_kappa6C_test.h"
#include "constants.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa6C_Test );

void
PseudoAxe_Kappa6C_Test::setUp(void)
{ 
    m_geometry_E4C = new geometry::eulerian4C::Vertical;
    m_geometry_E6C = new geometry::Eulerian6C;

    m_alpha = 50. * hkl::constant::math::degToRad;
    m_geometry_K4C = new geometry::kappa4C::Vertical(m_alpha);
    m_geometry = new geometry::Kappa6C(m_alpha);

    m_geometry->get_source().setWaveLength(1.54);
}

void 
PseudoAxe_Kappa6C_Test::tearDown(void)
{
    delete m_geometry;
    delete m_geometry_E6C;
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
        omega.set_value(*m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, omega.get_value(*m_geometry), constant::math::epsilon_0);
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
    CPPUNIT_ASSERT_THROW(pseudo.set_value(*m_geometry, angle), HKLException);

    for(i=-chi_max;i<chi_max;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(*m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry), constant::math::epsilon_0);
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
        pseudo.set_value(*m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(*m_geometry), constant::math::epsilon_0);
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
    m_geometry->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry);

    //set_value test1 non degenerate case
    psi.set_value(*m_geometry, 0. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry, true);
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
    m_geometry->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry);
    psi.set_value(*m_geometry, 0. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry, true);
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
    m_geometry->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if ((i <= -174) || (i >= 47))
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(*m_geometry, angle), HKLException);
          }
        else
          {
            psi.set_value(*m_geometry, angle);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(*m_geometry), constant::math::epsilon_0);
          }
      }

    m_geometry_E4C->setAngles(30. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              60. * constant::math::degToRad);
    m_geometry_K4C->setFromGeometry(*m_geometry_E4C, true);
    m_geometry->setFromGeometry(*m_geometry_K4C, true);
    psi.initialize(*m_geometry);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if (abs(i) > 100)
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(*m_geometry, angle), HKLException);
          }
        else
          {
            psi.set_value(*m_geometry, angle);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(*m_geometry), constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa6C_Test::Tth(void)
{
    hkl::pseudoAxe::kappa6C::eulerian6C::Tth pseudoAxe;

    // exception if not initialize
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(*m_geometry));
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(*m_geometry, 1), HKLException);
    m_geometry->setAngles(0, 0, 0, 0, 0, 1);
    pseudoAxe.initialize(*m_geometry);

    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(*m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(*m_geometry, 34. * constant::math::degToRad));

    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry->setAngles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
        pseudoAxe.initialize(*m_geometry);
        for(j=0;j<100;j++)
          {
            double angle0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
            pseudoAxe.set_value(*m_geometry, angle0);
            double angle = pseudoAxe.get_value(*m_geometry);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi), constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa6C_Test::Q(void)
{
    hkl::pseudoAxe::kappa6C::eulerian6C::Q pseudoAxe;

    // no exception if now initialize this pseudoAxe is always valid.
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(*m_geometry));
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(*m_geometry, 1), HKLException);

    m_geometry->setAngles(0, 0, 0, 0, 0, 1);
    pseudoAxe.initialize(*m_geometry);

    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(*m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(*m_geometry, 34. * constant::math::degToRad));

    double lambda = m_geometry->get_source().get_waveLength();
    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry->setAngles(mu0, komega0, kappa0, kphi0, gamma0, delta0);
        pseudoAxe.initialize(*m_geometry);
        for(j=0;j<100;j++)
          {
            double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
            double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
            pseudoAxe.set_value(*m_geometry, q0);
            double q = pseudoAxe.get_value(*m_geometry);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
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
