#include "pseudoaxe_eulerian4C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian4C_Vertical_Test );

void
PseudoAxe_Eulerian4C_Vertical_Test::setUp(void)
{ 
    //m_geometry.get_source().setWaveLength(1.54);
    m_geometry = geometry::eulerian4C::Vertical();
    m_geometry.setAngles(45. * constant::math::degToRad,
                         77. * constant::math::degToRad,
                         -5. * constant::math::degToRad,
                         34. * constant::math::degToRad);  
}

void 
PseudoAxe_Eulerian4C_Vertical_Test::tearDown(void)
{}

void 
PseudoAxe_Eulerian4C_Vertical_Test::Psi(void)
{
    int i;
    double angle = 10. * hkl::constant::math::degToRad;
    hkl::pseudoAxe::eulerian4C::vertical::Psi psi(m_geometry);

    //Can not initialize if the wavelength is not properly set.
    CPPUNIT_ASSERT_THROW(psi.initialize(), HKLException);
    CPPUNIT_ASSERT_THROW(psi.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(psi.set_value(0.), HKLException);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, psi.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, psi.get_max(), constant::math::epsilon_0);
    // pseudoAxe must be non-writable.
    CPPUNIT_ASSERT_EQUAL(false, psi.get_writable());

    // no more exception with a good wave length
    m_geometry.get_source().setWaveLength(1.54);
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    //set_value test2 degenerate case
    m_geometry.setAngles(30 * constant::math::degToRad,
                         0 * constant::math::degToRad,
                         0 * constant::math::degToRad,
                         60 * constant::math::degToRad);
    psi.initialize();
    psi.set_value(0. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    // exception if the current geometry is not compatible with the initialization
    m_geometry.setAngles(1, 0, 0, 0);
    CPPUNIT_ASSERT_THROW(psi.get_value(), HKLException);
    // the pseudoAxe must be non-writable
    CPPUNIT_ASSERT_EQUAL(false, psi.get_writable());

    //random test1
    m_geometry.setAngles(45 * constant::math::degToRad,
                         77 * constant::math::degToRad,
                         180 * constant::math::degToRad,
                         34 * constant::math::degToRad);
    psi.initialize();
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        CPPUNIT_ASSERT_NO_THROW(psi.set_value(angle));
        // the pseudoAxe must bez writable
        CPPUNIT_ASSERT_EQUAL(true, psi.get_writable());
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, psi.get_value(), constant::math::epsilon_0);
        // get_value change the writable state of the pseudoAxe.
        CPPUNIT_ASSERT_EQUAL(true, psi.get_writable());
      }

    //random test2
    m_geometry.setAngles(30 * constant::math::degToRad,
                         0 * constant::math::degToRad,
                         0 * constant::math::degToRad,
                         60 * constant::math::degToRad);
    psi.initialize();
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        psi.set_value(angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle,
                                     psi.get_value(),
                                     constant::math::epsilon_0);
      }
}

void 
PseudoAxe_Eulerian4C_Vertical_Test::Th2th(void)
{
    hkl::pseudoAxe::eulerian4C::vertical::twoC::Th2th pseudoAxe(m_geometry);

    // exception if the source is not well set
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0, pseudoAxe.get_max(), constant::math::epsilon_0);

    // no more exception after initialization of the source.
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(1 * constant::math::degToRad));
    CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_min(), pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_max(), pseudoAxe.get_max(), constant::math::epsilon_0);

    // test the uninitialize
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
    // this pseudoAxe can be read all the time when the source is well set.
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1), HKLException);
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
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, pseudoAxe.get_value(), constant::math::epsilon_0);


    //set_value
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(36. * constant::math::degToRad));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(46 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    // put a non-compatible geometry and test the unactivation of the pseudoAxe.
    m_geometry.setAngles(0, 0, 0, 1);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_min(), pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_max(), pseudoAxe.get_max(), constant::math::epsilon_0);

    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(omega0, chi0, phi0, tth0);
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
PseudoAxe_Eulerian4C_Vertical_Test::Q2th(void)
{
    hkl::pseudoAxe::eulerian4C::vertical::twoC::Q2th pseudoAxe(m_geometry);

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
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(value, pseudoAxe.get_value(), constant::math::epsilon_0);


    //set_value
    theta = 36 / 2;
    value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
    pseudoAxe.set_value(value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(46 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);

    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(omega0, chi0, phi0, tth0);
        pseudoAxe.initialize();
        for(j=0;j<100;j++)
          {
            double theta = constant::math::pi * (rand() / (RAND_MAX + 1.) - 1./2.);
            double q0 = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
            pseudoAxe.set_value(q0);
            double q = pseudoAxe.get_value();
            CPPUNIT_ASSERT_DOUBLES_EQUAL(q0, q, constant::math::epsilon_0);
          }
      }
}

void 
PseudoAxe_Eulerian4C_Vertical_Test::Q(void)
{
    hkl::pseudoAxe::eulerian4C::vertical::twoC::Q pseudoAxe(m_geometry);

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
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(11 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
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
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(10 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(11 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(omega0, chi0, phi0, tth0);
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
PseudoAxe_Eulerian4C_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::eulerian4C::vertical::Psi psi_ref(m_geometry);
    hkl::pseudoAxe::eulerian4C::vertical::Psi psi(m_geometry);
    stringstream flux;

    psi_ref.toStream(flux);

    psi.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
}
