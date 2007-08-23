// File to test matrix and vector implementation.
#include "source_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( sourceTest );

void
sourceTest::setUp(void)
{
  m_v = svector(1., 0., 0.);
}

void
sourceTest::tearDown(void)
{}

void
sourceTest::Constructor(void)
{
  const Source source(1., m_v);

  CPPUNIT_ASSERT_EQUAL(Value(1.), source.get_waveLength());
  CPPUNIT_ASSERT_EQUAL(m_v, source.get_direction());
}

void
sourceTest::Equal(void)
{
  const Source s(1., m_v);

  CPPUNIT_ASSERT_EQUAL(s, s);
}

void
sourceTest::CopyConstructor(void)
{
  const Source s1(1., m_v);
  const Source s2(s1);

  CPPUNIT_ASSERT_EQUAL(s1, s2);
}

void
sourceTest::SetWaveLength(void)
{
  Source s(1.54, m_v);

  CPPUNIT_ASSERT_THROW(s.setWaveLength(0.0), HKLException);

  CPPUNIT_ASSERT_NO_THROW(s.setWaveLength(1.));
  CPPUNIT_ASSERT_EQUAL(Value(1.), s.get_waveLength());

}

void
sourceTest::SetDirection(void)
{
  Source s(1., m_v);

  CPPUNIT_ASSERT_THROW(s.setDirection(svector()), HKLException);
  CPPUNIT_ASSERT_NO_THROW(s.setDirection(svector(1., 1., 0)));

  CPPUNIT_ASSERT_EQUAL(svector(1., 1., 0.).normalize(), s.get_direction());
}

void
sourceTest::GetSetKi(void)
{
  Source s(1.54, m_v);

  svector ki_ref(m_v);
  ki_ref *= constant::physic::tau / 1.54;
  CPPUNIT_ASSERT_EQUAL(ki_ref, s.getKi());

  CPPUNIT_ASSERT_THROW(s.setKi(svector()), HKLException);
  CPPUNIT_ASSERT_NO_THROW(s.setKi(svector(1., 1., 0.)));
  CPPUNIT_ASSERT_EQUAL(svector(1., 1., 0.), s.getKi());
}

void
sourceTest::persistanceIO(void)
{
  Source source_ref(1.54, svector(1e-8, 2, -3e14));
  Source source1_ref(1.54, svector(0, 2, 1));
  Source source;
  Source source1;
  std::stringstream flux;

  source_ref.toStream(flux);
  source.fromStream(flux);
  CPPUNIT_ASSERT_EQUAL(source_ref, source);

  source = Source();

  source_ref.toStream(flux);
  source1_ref.toStream(flux);
  source.fromStream(flux);
  source1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(source_ref, source);
  CPPUNIT_ASSERT_EQUAL(source1_ref, source1);
}
