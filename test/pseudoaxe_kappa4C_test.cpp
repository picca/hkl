#include "pseudoaxe_kappa4C_test.h"
#include "constants.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Kappa4C_Vertical_Test );

void
PseudoAxe_Kappa4C_Vertical_Test::setUp(void)
{
  m_geometry = geometry::kappa4C::Vertical();
  m_geometry_E4C = geometry::eulerian4C::Vertical();
}

void 
PseudoAxe_Kappa4C_Vertical_Test::tearDown(void)
{}

void 
PseudoAxe_Kappa4C_Vertical_Test::Omega(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::vertical::Omega omega;

    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        omega.set_value(m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, omega.get_value(m_geometry), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Chi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::vertical::Chi pseudo;
    int chi_max = 100;

    //test exception if chi > 2*alpha
    angle = chi_max + 0.1;
    CPPUNIT_ASSERT_THROW(pseudo.set_value(m_geometry, angle), HKLException);

    for(i=-chi_max;i<chi_max;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(m_geometry), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Phi(void)
{
    int i;
    double angle;
    hkl::pseudoAxe::kappa4C::vertical::Phi pseudo;

    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudo.set_value(m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudo.get_value(m_geometry), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Psi(void)
{
    int i;
    double angle = 10. * hkl::constant::math::degToRad;
    hkl::pseudoAxe::kappa4C::vertical::eulerian4C::Psi psi;

    m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                              77. * constant::math::degToRad,
                              -5. * constant::math::degToRad,
                              34. * constant::math::degToRad);  
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    // exception if the waveLength is null
    CPPUNIT_ASSERT_THROW(psi.initialize(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(psi.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(psi.set_value(m_geometry, 1), HKLException);

    // no more exception when the wave length is correct
    m_geometry_E4C.get_source().setWaveLength(1.54);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    CPPUNIT_ASSERT_NO_THROW(psi.initialize(m_geometry));

    //set_value test1 non degenerate case
    psi.set_value(m_geometry, 0. * constant::math::degToRad);
    m_geometry_E4C.setFromGeometry(m_geometry, true);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45. * constant::math::degToRad, m_geometry_E4C.get_axe("omega").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77. * constant::math::degToRad, m_geometry_E4C.get_axe("chi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5. * constant::math::degToRad, m_geometry_E4C.get_axe("phi").get_value(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, m_geometry_E4C.get_axe("2theta").get_value(), constant::math::epsilon_0);

    //set_value test2 degenerate case
    m_geometry_E4C.setAngles(30. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              60. * constant::math::degToRad);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    psi.initialize(m_geometry);
    psi.set_value(m_geometry, 0. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30. * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60. * constant::math::degToRad,
                                 m_geometry_E4C.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    //get_value test
    m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                              77. * constant::math::degToRad,
                              180. * constant::math::degToRad,
                              34. * constant::math::degToRad);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    psi.initialize(m_geometry);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if ((i <= -174) || (i >= 47))
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(m_geometry, angle), HKLException);
          }
        else
          {
            psi.set_value(m_geometry, angle);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(m_geometry), constant::math::epsilon_0);
          }
      }

    m_geometry_E4C.setAngles(30. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              0. * constant::math::degToRad,
                              60. * constant::math::degToRad);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    psi.initialize(m_geometry);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if (abs(i) > 100)
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(m_geometry, angle), HKLException);
          }
        else
          {
            psi.set_value(m_geometry, angle);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(m_geometry), constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Th2th(void)
{
    hkl::pseudoAxe::kappa4C::vertical::twoC::Th2th pseudoAxe;

    // exception if not initialize
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);

    m_geometry.setAngles(45. * constant::math::degToRad,
                          77. * constant::math::degToRad,
                          -5. * constant::math::degToRad,
                          34. * constant::math::degToRad);  
    pseudoAxe.initialize(m_geometry);

    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 1. * constant::math::degToRad));

    //set_value
    pseudoAxe.set_value(m_geometry, 34. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);


    //set_value
    pseudoAxe.set_value(m_geometry, 36. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(46 * constant::math::degToRad,
                                 m_geometry.get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<10;i++)
      {
        double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
        pseudoAxe.initialize(m_geometry);
        for(j=0;j<100;j++)
          {
            double angle0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
            pseudoAxe.set_value(m_geometry, angle0);
            double angle = pseudoAxe.get_value(m_geometry);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi), constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Q2th(void)
{
    hkl::pseudoAxe::kappa4C::vertical::twoC::Q2th pseudoAxe;

    // exception if not initialize
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);
    m_geometry.setAngles(45. * constant::math::degToRad,
                          77. * constant::math::degToRad,
                          -5. * constant::math::degToRad,
                          34. * constant::math::degToRad);  
    // exception because the wave length is not properly set.
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(m_geometry), HKLException);

    // no more exception after wave length is properly set
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize(m_geometry));
    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 1. * constant::math::degToRad));

    //set_value
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2;
    double value = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(value, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);

    //set_value
    theta = 36 / 2;
    value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(46 * constant::math::degToRad,
                                 m_geometry.get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
        pseudoAxe.initialize(m_geometry);
        for(j=0;j<100;j++)
          {
            double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
            double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
            pseudoAxe.set_value(m_geometry, q0);
            double q = pseudoAxe.get_value(m_geometry);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Q(void)
{
    hkl::pseudoAxe::kappa4C::vertical::twoC::Q pseudoAxe;

    // exception if the wave length is not set properly
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);
    // no need to initialize but check that the wave length is not correct.
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(m_geometry), HKLException);

    // no exception if not initialize this pseudoAxe is always valid.
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 1. * constant::math::degToRad));
    // no need to initialize but check that the wave length is correct.
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize(m_geometry));


    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 1. * constant::math::degToRad));

    m_geometry.setAngles(45 * constant::math::degToRad,
                          10 * constant::math::degToRad,
                          11 * constant::math::degToRad,
                          34 * constant::math::degToRad);
    //set_value
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2 * constant::math::degToRad;
    double value = 2 * constant::physic::tau * sin(theta) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * constant::math::degToRad,
                                 m_geometry.get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(11 * constant::math::degToRad,
                                 m_geometry.get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(value, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);


    //set_value
    theta = 36 / 2;
    value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("komega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * constant::math::degToRad,
                                 m_geometry.get_axe("kappa").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(11 * constant::math::degToRad,
                                 m_geometry.get_axe("kphi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double komega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kappa0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double kphi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(komega0, kappa0, kphi0, tth0);
        pseudoAxe.initialize(m_geometry);
        for(j=0;j<100;j++)
          {
            double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
            double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
            pseudoAxe.set_value(m_geometry, q0);
            double q = pseudoAxe.get_value(m_geometry);
            CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
          }
      }
}

void
PseudoAxe_Kappa4C_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::kappa4C::vertical::Omega omega_ref;
    hkl::pseudoAxe::kappa4C::vertical::Omega omega;
    stringstream flux;

    omega_ref.toStream(flux);

    omega.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(omega_ref, omega);
}
