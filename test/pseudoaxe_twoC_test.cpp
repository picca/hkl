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
  hkl::pseudoAxeEngine::twoC::vertical::Th2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0]; 

  // this pseudoAxe is always valid.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  // exception if the source is not initialized
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());

  // no more exception after initialization of the source.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1 * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());

  // set a non valid geometry an test
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(0, 1));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());

  // test the uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe can be read all the time when the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry._tth->get_max(), pseudoAxe.get_max());

  //set_current
  m_geometry.setAngles(45 * constant::math::degToRad,
                       34 * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value(34. * constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(36. * constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(Value(46 * constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(omega0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(Value(angle0)));
          double angle = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_EQUAL(fmod(angle0, constant::math::pi), fmod(angle, constant::math::pi));
        }
    }

#ifdef PROFILE
  // profiling
  Value v(36. * constant::math::degToRad);
  for(unsigned int i=0;i<1000000; i++)
    {
      pseudoAxe.set_current(v);
      pseudoAxe.get_current();
      pseudoAxe.get_min();
      pseudoAxe.get_max();
    }
#endif
}

void
PseudoAxe_TwoC_Vertical_Test::Q2th(void)
{
  hkl::pseudoAxeEngine::twoC::vertical::Q2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0];

  // exception if not initialize
  // This pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // This pseudoAxe can be read all the time one the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2;
  double value = 2 * constant::physic::tau * sin(theta * constant::math::degToRad) / lambda;
  m_geometry.setAngles(45 * constant::math::degToRad,
                       34 * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value(value), pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  pseudoAxe.set_current(value);
  CPPUNIT_ASSERT_EQUAL(Value(46 * constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

#ifdef PROFILE
  // profiling
  Value v(36. * constant::math::degToRad);
  for(unsigned int i=0;i<1000000; i++)
    {
      pseudoAxe.set_current(v);
      pseudoAxe.get_current();
      pseudoAxe.get_min();
      pseudoAxe.get_max();
    }
#endif

  // if put a non valid geometry can not set the value.
  m_geometry.setAngles(40. * constant::math::degToRad,
                       30. * constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * constant::math::degToRad), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

}

void
PseudoAxe_TwoC_Vertical_Test::Q(void)
{
  hkl::pseudoAxeEngine::twoC::vertical::Q pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()[0];

  // exception if not initialized.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.get_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.get_readable());
  CPPUNIT_ASSERT_EQUAL(Value(-2 * constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(Value(2 * constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  m_geometry.setAngles(45 * constant::math::degToRad, 34 * constant::math::degToRad);
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2 * constant::math::degToRad;
  double value = 2 * constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(34 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(Value(value), pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * constant::physic::tau * sin(theta* constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(Value(45 * constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(Value(36 * constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

#ifdef PROFILE
  // profiling
  Value v(36. * constant::math::degToRad);
  for(unsigned int i=0;i<1000000; i++)
    {
      pseudoAxe.set_current(v);
      pseudoAxe.get_current();
      pseudoAxe.get_min();
      pseudoAxe.get_max();
    }
#endif
}

void
PseudoAxe_TwoC_Vertical_Test::persistanceIO(void)
{
  hkl::pseudoAxeEngine::twoC::vertical::Th2th th2th_ref(m_geometry);
  hkl::pseudoAxeEngine::twoC::vertical::Th2th th2th(m_geometry);

  stringstream flux;

  th2th_ref.toStream(flux);

  th2th.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
}
