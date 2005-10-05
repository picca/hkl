// File to test matrix and vector implementation.
#include "source_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( sourceTest );

void
sourceTest::setUp()
{
  m_v = svector(1., 0., 0.);
}

void 
sourceTest::tearDown() 
{
}

void 
sourceTest::Constructor()
{
  const Source source(1., m_v);
  
  CPPUNIT_ASSERT_EQUAL(1., source.get_waveLength());
  CPPUNIT_ASSERT_EQUAL(m_v, source.get_direction());
}

void 
sourceTest::Equal()
{
  const Source s(1., m_v);
  
  CPPUNIT_ASSERT_EQUAL(s, s);
}

void
sourceTest::CopyConstructor()
{
  const Source s1(1., m_v);
  const Source s2(s1);
  
  CPPUNIT_ASSERT_EQUAL(s1, s2);
}

void
sourceTest::SetWaveLength()
{
  Source s(1.54, m_v);
  
  s.setWaveLength(1.);
  
  CPPUNIT_ASSERT_EQUAL(1., s.get_waveLength());
}

void
sourceTest::SetDirection()
{
  Source s(1., m_v);
  
  s.setDirection(svector(1., 1., 0));
  
  CPPUNIT_ASSERT_EQUAL(svector(1., 1., 0.).normalize(), s.get_direction());
}

void
sourceTest::GetSetKi()
{
  Source s(1.54, m_v);
 
  svector ki_ref(m_v);
  ki_ref *= constant::physic::tau / 1.54;
  CPPUNIT_ASSERT_EQUAL(ki_ref, s.getKi());
  
  s.setKi(svector(1., 1., 0.));
  CPPUNIT_ASSERT_EQUAL(svector(1., 1., 0.), s.getKi());
}
