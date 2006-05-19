#include "pseudoaxe_twoC_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_TwoC_Vertical_Test );

void
PseudoAxe_TwoC_Vertical_Test::setUp(void)
{ 
}

void 
PseudoAxe_TwoC_Vertical_Test::tearDown(void)
{}

void 
PseudoAxe_TwoC_Vertical_Test::Th2th(void)
{
    hkl::pseudoAxe::twoC::vertical::Th2th pseudoAxe;

    // exception if now initialize
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);

    // no exception for the initialization
    m_geometry.get_axe("omega").set_value(45. * constant::math::degToRad);
    m_geometry.get_axe("2theta").set_value(34. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize(m_geometry));

    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 1. * constant::math::degToRad));

    //set_value
    pseudoAxe.set_value(m_geometry, 34. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, pseudoAxe.get_value(m_geometry), constant::math::epsilon_0);


    //set_value
    pseudoAxe.set_value(m_geometry, 36. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(46 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
}

void 
PseudoAxe_TwoC_Vertical_Test::Q2th(void)
{
    hkl::pseudoAxe::twoC::vertical::Q2th pseudoAxe;

    // exception if not initialize
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);

    // exception if the wave length is not properly set
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(m_geometry), HKLException);
    m_geometry.get_source().setWaveLength(1.54);
    // no more exception after wave length initialization.
    m_geometry.get_axe("omega").set_value(45. * constant::math::degToRad);
    m_geometry.get_axe("2theta").set_value(34. * constant::math::degToRad);  
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
                                 m_geometry.get_axe("omega").get_value(),
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
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
}

void 
PseudoAxe_TwoC_Vertical_Test::Q(void)
{
    hkl::pseudoAxe::twoC::vertical::Q pseudoAxe;

    // exception if the wavelength is not set properly
    CPPUNIT_ASSERT_THROW(pseudoAxe.get_value(m_geometry), HKLException);
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(m_geometry, 1), HKLException);
    
    // exception if the wave length is not properly set
    CPPUNIT_ASSERT_THROW(pseudoAxe.initialize(m_geometry), HKLException);
    m_geometry.get_source().setWaveLength(1.54);
    // no more exception after wave length initialization.
    m_geometry.get_axe("omega").set_value(45. * constant::math::degToRad);
    m_geometry.get_axe("2theta").set_value(34. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize(m_geometry));

    // no more exception after initialization
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value(m_geometry));
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(m_geometry, 1. * constant::math::degToRad));

    m_geometry.setAngles(45 * constant::math::degToRad, 34 * constant::math::degToRad);
    //set_value
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2 * constant::math::degToRad;
    double value = 2 * constant::physic::tau * sin(theta) / lambda;
    pseudoAxe.set_value(m_geometry, value);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
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
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
}

void
PseudoAxe_TwoC_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::twoC::vertical::Th2th th2th_ref, th2th;
    stringstream flux;

    th2th_ref.toStream(flux);

    th2th.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
}
