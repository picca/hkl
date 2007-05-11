#include "pseudoaxe_twoC_test.h"
#include "pseudoaxe.h"
#include <fstream>

CPPUNIT_TEST_SUITE_REGISTRATION( PseudoAxe_TwoC_Vertical_Test );

void
PseudoAxe_TwoC_Vertical_Test::setUp(void)
{
  m_geometry = hkl::twoC::vertical::Geometry();
}

void
PseudoAxe_TwoC_Vertical_Test::tearDown(void)
{}

void
PseudoAxe_TwoC_Vertical_Test::Th2th(void)
{
  hkl::twoC::vertical::pseudoAxeEngine::Th2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["th2th"];

  // this pseudoAxe is always valid.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  // exception if the source is not initialized
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());

  // no more exception after initialization of the source.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(1 * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());

  // set a non valid geometry an test
  CPPUNIT_ASSERT_NO_THROW(m_geometry.setAngles(0, 1));
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());

  // test the uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // this pseudoAxe can be read all the time when the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_min(), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(m_geometry.tth()->get_max(), pseudoAxe.get_max());

  //set_current
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(34. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34. * hkl::constant::math::degToRad), pseudoAxe.get_current());


  //set_current
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(36. * hkl::constant::math::degToRad));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // test the set_write_from_read
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);

  // random test
  unsigned int i;
  unsigned int j;
  for(i=0;i<100;i++)
    {
      double omega0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      double tth0 = hkl::constant::math::pi * (2. * rand() / (RAND_MAX + 1.) - 1.);
      m_geometry.setAngles(omega0, tth0);
      CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
      double min = pseudoAxe.get_min().get_value();
      double max = pseudoAxe.get_max().get_value();
      for(j=0;j<100;j++)
        {
          double angle0 = (max - min) * rand() / (RAND_MAX + 1.) + min;
          CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(hkl::Value(angle0)));
          double angle = pseudoAxe.get_current().get_value();
          CPPUNIT_ASSERT_EQUAL(fmod(angle0, hkl::constant::math::pi), fmod(angle, hkl::constant::math::pi));
        }
    }

#ifdef PROFILE
  // profiling
  hkl::Value v(36. * hkl::constant::math::degToRad);
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
  hkl::twoC::vertical::pseudoAxeEngine::Q2th pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q2th"];

  // exception if not initialize
  // This pseudoAxe can be read all the time.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after the source initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  // This pseudoAxe can be read all the time one the source is well set.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2;
  double value = 2 * hkl::constant::physic::tau * sin(theta * hkl::constant::math::degToRad) / lambda;
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL((hkl::Value)value, pseudoAxe.get_current());


  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  pseudoAxe.set_current(value);
  CPPUNIT_ASSERT_EQUAL(hkl::Value(46 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // test the set_write_from_read
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);

#ifdef PROFILE
  // profiling
  hkl::Value v(36. * hkl::constant::math::degToRad);
  for(unsigned int i=0;i<1000000; i++)
    {
      pseudoAxe.set_current(v);
      pseudoAxe.get_current();
      pseudoAxe.get_min();
      pseudoAxe.get_max();
    }
#endif

  // if put a non valid geometry can not set the value.
  m_geometry.setAngles(40. * hkl::constant::math::degToRad,
                       30. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(1. * hkl::constant::math::degToRad), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

}

void
PseudoAxe_TwoC_Vertical_Test::Q(void)
{
  hkl::twoC::vertical::pseudoAxeEngine::Q pseudoAxeEngine(m_geometry);
  hkl::PseudoAxe & pseudoAxe = *pseudoAxeEngine.pseudoAxes()["q"];

  // exception if not initialized.
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // no more exception after initialisation
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(0));
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  // uninitialize
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.uninitialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.get_current());
  CPPUNIT_ASSERT_THROW(pseudoAxe.set_current(0), hkl::HKLException);
  CPPUNIT_ASSERT_EQUAL(false, pseudoAxe.is_writable());
  CPPUNIT_ASSERT_EQUAL(true, pseudoAxe.is_readable());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(-2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_min());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(2 * hkl::constant::physic::tau / 1.54), pseudoAxe.get_max());

  //set_current
  m_geometry.setAngles(45 * hkl::constant::math::degToRad, 34 * hkl::constant::math::degToRad);
  double lambda = m_geometry.get_source().get_waveLength().get_value();
  double theta = 34 / 2 * hkl::constant::math::degToRad;
  double value = 2 * hkl::constant::physic::tau * sin(theta) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(34 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());
  //get_value
  CPPUNIT_ASSERT_EQUAL((hkl::Value)value, pseudoAxe.get_current());

  //set_current
  theta = 36 / 2;
  value = 2 * hkl::constant::physic::tau * sin(theta* hkl::constant::math::degToRad) / lambda;
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(value));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("omega").get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(36 * hkl::constant::math::degToRad),
                       m_geometry.get_axe("2theta").get_current());

  // test the set_write_from_read
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.initialize());
  hkl::Value read;
  hkl::Value write;
  // after an initialization the read and the write part must be identical.
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be equal
  CPPUNIT_ASSERT_NO_THROW(pseudoAxe.set_current(32. * hkl::constant::math::degToRad));
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);
  // must be non-equal
  m_geometry.setAngles(45 * hkl::constant::math::degToRad,
                       34 * hkl::constant::math::degToRad);
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(read, write));
  pseudoAxe.set_write_from_read();
  pseudoAxe.get_read_write(read, write);
  CPPUNIT_ASSERT_EQUAL(read, write);

#ifdef PROFILE
  // profiling
  hkl::Value v(36. * hkl::constant::math::degToRad);
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
  hkl::twoC::vertical::pseudoAxeEngine::Th2th th2th_ref(m_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Th2th th2th(m_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q2th q2th_ref(m_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q2th q2th(m_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q q_ref(m_geometry);
  hkl::twoC::vertical::pseudoAxeEngine::Q q(m_geometry);

  stringstream flux;

  th2th_ref.toStream(flux);
  q2th_ref.toStream(flux);
  q_ref.toStream(flux);

  th2th.fromStream(flux);
  q2th.fromStream(flux);
  q.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(th2th_ref, th2th);
  CPPUNIT_ASSERT_EQUAL(q2th_ref, q2th);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}
