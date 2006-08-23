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

    m_geometry.get_source().setWaveLength(1.54);
    hkl::pseudoAxe::kappa4C::vertical::Omega pseudoAxe(m_geometry);

    // test the initial state of the pseudoAxe
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-constant::math::pi, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::math::pi, pseudoAxe.get_max(), constant::math::epsilon_0);

    // uninitialize it
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0.), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // initialize it
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-constant::math::pi, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::math::pi, pseudoAxe.get_max(), constant::math::epsilon_0);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(angle));
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_value(), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Chi(void)
{
    int i;
    double angle;

    m_geometry.get_source().setWaveLength(1.54);
    hkl::pseudoAxe::kappa4C::vertical::Chi pseudoAxe(m_geometry);
    int chi_max = 100;

    // test the initial state of the pseudoAxe
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-m_geometry.get_alpha() * 2, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.get_alpha() * 2, pseudoAxe.get_max(), constant::math::epsilon_0);

    // uninitialize it
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0.), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // initialize it
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-m_geometry.get_alpha() * 2, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.get_alpha() * 2, pseudoAxe.get_max(), constant::math::epsilon_0);
    //test exception if chi > 2*alpha
    angle = chi_max + 0.1;
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(angle), HKLException);

    for(i=-chi_max;i<chi_max;i++)
      {
        angle = i * constant::math::degToRad;
        CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(angle));
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_value(), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Phi(void)
{
    int i;
    double angle;

    m_geometry.get_source().setWaveLength(1.54);
    hkl::pseudoAxe::kappa4C::vertical::Phi pseudoAxe(m_geometry);

    // test the initial state of the pseudoAxe
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-constant::math::pi, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::math::pi, pseudoAxe.get_max(), constant::math::epsilon_0);

    // uninitialize it
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0.), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // initialize it
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-constant::math::pi, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::math::pi, pseudoAxe.get_max(), constant::math::epsilon_0);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(angle));
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_value(), constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Psi(void)
{
    int i;
    double angle = 10. * hkl::constant::math::degToRad;
    hkl::pseudoAxe::kappa4C::vertical::eulerian4C::Psi psi(m_geometry);

    m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                             77. * constant::math::degToRad,
                             -5. * constant::math::degToRad,
                             34. * constant::math::degToRad);  
    m_geometry.setFromGeometry(m_geometry_E4C, true);

    //Can not initialize if the wavelength is not properly set.
    CPPUNIT_ASSERT_THROW(psi.initialize(), HKLException);
    CPPUNIT_ASSERT_THROW(psi.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(psi.set_value(1.), HKLException);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, psi.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, psi.get_max(), constant::math::epsilon_0);
    // pseudoAxe must be non-writable.
    CPPUNIT_ASSERT_EQUAL(false, psi.get_writable());

    // no more exception when the wave length is correct
    m_geometry_E4C.get_source().setWaveLength(1.54);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    CPPUNIT_ASSERT_NO_THROW(psi.initialize());
    // the pseudoAxe must be writable
    CPPUNIT_ASSERT_EQUAL(true, psi.get_writable());
    CPPUNIT_ASSERT_NO_THROW(psi.set_value(0. * constant::math::degToRad));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-constant::math::pi, psi.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::math::pi, psi.get_max(), constant::math::epsilon_0);
    // exception after uninitialization
    CPPUNIT_ASSERT_NO_THROW(psi.uninitialize());
    CPPUNIT_ASSERT_THROW(psi.set_value(0. * constant::math::degToRad), HKLException);

    //set_value test1 non degenerate case
    CPPUNIT_ASSERT_NO_THROW(psi.initialize());
    CPPUNIT_ASSERT_NO_THROW(psi.set_value(0. * constant::math::degToRad));
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
    CPPUNIT_ASSERT_NO_THROW(psi.initialize());
    CPPUNIT_ASSERT_NO_THROW(psi.set_value(0. * constant::math::degToRad));
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

    // exception if the current geometry is not compatible with the initialization
    m_geometry.setAngles(1, 0, 0, 0);
    CPPUNIT_ASSERT_THROW(psi.get_value(), HKLException);
    // the pseudoAxe must be non-writable
    CPPUNIT_ASSERT_EQUAL(false, psi.get_writable());

    //get_value test
    m_geometry_E4C.setAngles(45. * constant::math::degToRad,
                             77. * constant::math::degToRad,
                             180. * constant::math::degToRad,
                             34. * constant::math::degToRad);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    CPPUNIT_ASSERT_NO_THROW(psi.initialize());
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if ((i <= -174) || (i >= 47))
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(angle), HKLException);
          }
        else
          {
            CPPUNIT_ASSERT_NO_THROW(psi.set_value(angle));
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(), constant::math::epsilon_0);
          }
      }

    m_geometry_E4C.setAngles(30. * constant::math::degToRad,
                             0. * constant::math::degToRad,
                             0. * constant::math::degToRad,
                             60. * constant::math::degToRad);
    m_geometry.setFromGeometry(m_geometry_E4C, true);
    CPPUNIT_ASSERT_NO_THROW(psi.initialize());
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        if (abs(i) > 100)
          {
            CPPUNIT_ASSERT_THROW(psi.set_value(angle), HKLException);
          }
        else
          {
            CPPUNIT_ASSERT_NO_THROW(psi.set_value(angle));
            CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(), constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Th2th(void)
{
    hkl::pseudoAxe::kappa4C::vertical::twoC::Th2th pseudoAxe(m_geometry);

    // exception if the source is not preoerly set.
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // no more exception after the source initialisation
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(1. * constant::math::degToRad));
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_min(), pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_max(), pseudoAxe.get_max(), constant::math::epsilon_0);

    // uninitialize
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    // this pseudoAxe can be read all the time when the source is well set.
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1. * constant::math::degToRad), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_min(), pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_max(), pseudoAxe.get_max(), constant::math::epsilon_0);

    //set_value
    m_geometry.setAngles(45. * constant::math::degToRad,
                         77. * constant::math::degToRad,
                         -5. * constant::math::degToRad,
                         34. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(34. * constant::math::degToRad));
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, pseudoAxe.get_value(), constant::math::epsilon_0);


    //set_value
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(36. * constant::math::degToRad));
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

    // if put a non valid geometry can not set the value.
    m_geometry.setAngles(40. * constant::math::degToRad,
                         72. * constant::math::degToRad,
                         -1. * constant::math::degToRad,
                         30. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1. * constant::math::degToRad), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_min(), pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_max(), pseudoAxe.get_max(), constant::math::epsilon_0);

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
        CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
        for(j=0;j<100;j++)
          {
            double angle0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
            CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(angle0));
            double angle = pseudoAxe.get_value();
            CPPUNIT_ASSERT_DOUBLES_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi), constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Q2th(void)
{
    hkl::pseudoAxe::kappa4C::vertical::twoC::Q2th pseudoAxe(m_geometry);

    // exception if not initialize
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // exception if the source is not properly set.
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(), HKLException);

    // no more exception after the source initialisation
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(0));
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);

    // uninitialize
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    // This pseudoAxe can be read all the time one the source is well set.
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);

    //set_value
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2;
    double value = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
    m_geometry.setAngles(45. * constant::math::degToRad,
                         77. * constant::math::degToRad,
                         -5. * constant::math::degToRad,
                         34. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(value));
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(value, pseudoAxe.get_value(), constant::math::epsilon_0);

    //set_value
    theta = 36 / 2;
    value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(value));
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

    // if put a non valid geometry can not get the value.
    m_geometry.setAngles(40. * constant::math::degToRad,
                         72. * constant::math::degToRad,
                         -1. * constant::math::degToRad,
                         30. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1. * constant::math::degToRad), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);

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
        CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
        for(j=0;j<100;j++)
          {
            double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
            double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
            CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(q0));
            double q = pseudoAxe.get_value();
            CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Kappa4C_Vertical_Test::Q(void)
{
    hkl::pseudoAxe::kappa4C::vertical::twoC::Q pseudoAxe(m_geometry);

    // exception if the source is not properly set.
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // no more exception after the source initialisation
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(0));
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);

    // uninitialize
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(0), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);

    m_geometry.setAngles(45 * constant::math::degToRad,
                         10 * constant::math::degToRad,
                         11 * constant::math::degToRad,
                         34 * constant::math::degToRad);
    //set_value
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2 * constant::math::degToRad;
    double value = 2 * constant::physic::tau * sin(theta) / lambda;
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(value));
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(value, pseudoAxe.get_value(), constant::math::epsilon_0);


    //set_value
    theta = 36 / 2;
    value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(value));
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
        CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
        for(j=0;j<100;j++)
          {
            double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
            double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
            CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(q0));
            double q;
            CPPUNIT_ASSERT_NO_THROW(q = pseudoAxe.get_value());
            CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
          }
      }
}

void
PseudoAxe_Kappa4C_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::kappa4C::vertical::Omega omega_ref(m_geometry);
    hkl::pseudoAxe::kappa4C::vertical::Omega omega(m_geometry);
    stringstream flux;

    omega_ref.toStream(flux);

    omega.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(omega_ref, omega);
}
