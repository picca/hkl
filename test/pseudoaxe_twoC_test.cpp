#include "pseudoaxe_twoC_test.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_TwoC_Vertical_Test );

void
PseudoAxe_TwoC_Vertical_Test::setUp(void)
{
  m_geometry = geometry::twoC::Vertical();
}

void 
PseudoAxe_TwoC_Vertical_Test::tearDown(void)
{}

void 
PseudoAxe_TwoC_Vertical_Test::Th2th(void)
{
    hkl::pseudoAxe::twoC::vertical::Th2th pseudoAxe(m_geometry);

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
    m_geometry.setAngles(45 * constant::math::degToRad,
                         34 * constant::math::degToRad);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(34. * constant::math::degToRad));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
    //get_value
    CPPUNIT_ASSERT_DOUBLES_EQUAL(34. * constant::math::degToRad, pseudoAxe.get_value(), constant::math::epsilon_0);


    //set_value
    pseudoAxe.set_value(36. * constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(46 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
                                 constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    // put a non-compatible geometry and test the unactivation of the pseudoAxe.
    m_geometry.setAngles(0, 1);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_min(), pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(m_geometry.m_tth.get_max(), pseudoAxe.get_max(), constant::math::epsilon_0);

}

void 
PseudoAxe_TwoC_Vertical_Test::Q2th(void)
{
    hkl::pseudoAxe::twoC::vertical::Q2th pseudoAxe(m_geometry);

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
    m_geometry.setAngles(45 * constant::math::degToRad,
                         34 * constant::math::degToRad);
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(value));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);

    // if put a non valid geometry can not set the value.
    m_geometry.setAngles(40. * constant::math::degToRad,
                         30. * constant::math::degToRad);  
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_value());
    CPPUNIT_ASSERT_THROW(pseudoAxe.set_value(1. * constant::math::degToRad), HKLException);
    CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
    CPPUNIT_ASSERT_DOUBLES_EQUAL(-2 * constant::physic::tau / 1.54, pseudoAxe.get_min(), constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2 * constant::physic::tau / 1.54, pseudoAxe.get_max(), constant::math::epsilon_0);
}

void 
PseudoAxe_TwoC_Vertical_Test::Q(void)
{
    hkl::pseudoAxe::twoC::vertical::Q pseudoAxe(m_geometry);

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

    //set_value
    m_geometry.setAngles(45 * constant::math::degToRad, 34 * constant::math::degToRad);
    double lambda = m_geometry.get_source().get_waveLength();
    double theta = 34 / 2 * constant::math::degToRad;
    double value = 2 * constant::physic::tau * sin(theta) / lambda;
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
    CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_value(value));
    CPPUNIT_ASSERT_DOUBLES_EQUAL(45 * constant::math::degToRad,
                                 m_geometry.get_axe("omega").get_value(),
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
    CPPUNIT_ASSERT_DOUBLES_EQUAL(36 * constant::math::degToRad,
                                 m_geometry.get_axe("2theta").get_value(),
                                 constant::math::epsilon_0);
}

void
PseudoAxe_TwoC_Vertical_Test::persistanceIO(void)
{
    hkl::pseudoAxe::twoC::vertical::Th2th th2th_ref(m_geometry);
    hkl::pseudoAxe::twoC::vertical::Th2th th2th(m_geometry);

    stringstream flux;

    th2th_ref.toStream(flux);

    th2th.fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
}
