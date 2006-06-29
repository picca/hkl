#include "pseudoaxe_eulerian6C_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_Eulerian6C_Vertical_Test );

void
PseudoAxe_Eulerian6C_Vertical_Test::setUp(void)
{ 
    m_geometry.get_axe("mu").set_value(1. * constant::math::degToRad);
    m_geometry.get_axe("omega").set_value(45. * constant::math::degToRad);
    m_geometry.get_axe("chi").set_value(77. * constant::math::degToRad);
    m_geometry.get_axe("phi").set_value(-5. * constant::math::degToRad);
    m_geometry.get_axe("gamma").set_value(0. * constant::math::degToRad);
    m_geometry.get_axe("delta").set_value(34. * constant::math::degToRad);  
}

void 
PseudoAxe_Eulerian6C_Vertical_Test::tearDown(void)
{}

void 
PseudoAxe_Eulerian6C_Vertical_Test::Tth(void)
{
    hkl::pseudoAxe::eulerian6C::Tth pseudoAxe;
   
    // no exception the pseudoAxe can be read all the time.
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    // exception if not initialize we can not write before initialization
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);

    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(m_geometry), HKLException);

    // no more exception after a correct initialization
    m_geometry.get_source().setWaveLength(1.54);
    pseudoAxe.initialize(m_geometry);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 34. * constant::math::degToRad));

    pseudoAxe.set_value(m_geometry, pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1 * constant::math::degToRad,
                                 m_geometry.get_axe("mu").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("gamma").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("delta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);


    //set_value
    pseudoAxe.set_value(m_geometry, 36. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1 * constant::math::degToRad,
                                 m_geometry.get_axe("mu").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("gamma").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("delta").get_value(),
                                 constant::math::epsilon_0);

    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(mu0, omega0, chi0, phi0, gamma0, delta0);
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
PseudoAxe_Eulerian6C_Vertical_Test::Q(void)
{
    hkl::pseudoAxe::eulerian6C::Q pseudoAxe;

    // no exception if not initialize this pseudoAxe is always valid.
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);

    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(m_geometry), HKLException);

    // no more exception after initialization of the wavelength
    m_geometry.get_source().setWaveLength(1.54);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    // exception because not already initialize
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 34. * constant::math::degToRad), HKLException);
    // no more exception after initialization
    pseudoAxe.initialize(m_geometry);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 34. * constant::math::degToRad));

    //set_value
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2 * constant::math::degToRad;
    double value = 2 * constant::physic::tau * sin(theta) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1 * constant::math::degToRad,
                                 m_geometry.get_axe("mu").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("gamma").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("delta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(value, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);


    //set_value
    theta = 36 / 2;
    value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1 * constant::math::degToRad,
                                 m_geometry.get_axe("mu").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("gamma").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("delta").get_value(),
                                 constant::math::epsilon_0);

    // peticular test
    pseudoAxe.set_value(m_geometry, 0.);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);

    // random test
    unsigned int i;
    unsigned int j;
    for(i=0;i<100;i++)
      {
        double mu0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double chi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double phi0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double gamma0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        double delta0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
        m_geometry.setAngles(mu0, omega0, chi0, phi0, gamma0, delta0);
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
PseudoAxe_Eulerian6C_Vertical_Test::Psi(void)
{
    int i;
    double angle = 10. * hkl::constant::math::degToRad;
    hkl::pseudoAxe::eulerian6C::eulerian4C::vertical::Psi pseudoAxe;

    pseudoAxe.initialize(m_geometry);

    //set_value test1 non degenerate case
    pseudoAxe.set_value(m_geometry, 0. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1 * constant::math::degToRad,
                                 m_geometry.get_axe("mu").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(77 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-5 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("gamma").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("delta").get_value(),
                                 constant::math::epsilon_0);

    //set_value test2 degenerate case
    m_geometry = hkl::geometry::Eulerian6C(0 * constant::math::degToRad,
                                           30 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           60 * constant::math::degToRad);
    pseudoAxe.initialize(m_geometry);
    pseudoAxe.set_value(m_geometry, 0. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("mu").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(30 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("chi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("phi").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0 * constant::math::degToRad,
                                 m_geometry.get_axe("gamma").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60 * constant::math::degToRad,
                                 m_geometry.get_axe("delta").get_value(),
                                 constant::math::epsilon_0);

    //get_value test
    m_geometry = hkl::geometry::Eulerian6C(0 * constant::math::degToRad,
                                           45 * constant::math::degToRad,
                                           77 * constant::math::degToRad,
                                           180 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           34 * constant::math::degToRad);

    pseudoAxe.initialize(m_geometry);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudoAxe.set_value(m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);
      }

    m_geometry = hkl::geometry::Eulerian6C(0 * constant::math::degToRad,
                                           30 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           0 * constant::math::degToRad,
                                           60 * constant::math::degToRad);

    pseudoAxe.initialize(m_geometry);
    for(i=-180;i<180;i++)
      {
        angle = i * constant::math::degToRad;
        pseudoAxe.set_value(m_geometry, angle);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(angle,
                                     pseudoAxe.get_value(m_geometry),
                                     constant::math::epsilon_0);
      }
}

void
PseudoAxe_Eulerian6C_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::eulerian6C::eulerian4C::vertical::Psi psi_ref, psi;
    stringstream flux;

    psi_ref.toStream(flux);

    psi.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(psi_ref, psi);
}
