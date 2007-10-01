// File to test matrix and vector implementation.
#include "config.h"
#include "source_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( sourceTest );

void
sourceTest::setUp(void)
{
  _v.data[X] = 1;
  _v.data[Y] = 0;
  _v.data[Z] = 0;
}

void
sourceTest::tearDown(void) {}

void
sourceTest::Constructor(void)
{
  Source source(1., &_v);

  CPPUNIT_ASSERT_EQUAL(Value(1.), source.get_waveLength());
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&_v, source.get_direction()));
}

void
sourceTest::Equal(void)
{
  const Source s(1., &_v);

  CPPUNIT_ASSERT_EQUAL(s, s);
}

void
sourceTest::CopyConstructor(void)
{
  const Source s1(1., &_v);
  const Source s2(s1);

  CPPUNIT_ASSERT_EQUAL(s1, s2);
}

void
sourceTest::SetWaveLength(void)
{
  Source s(1.54, &_v);

  CPPUNIT_ASSERT_THROW(s.setWaveLength(0.0), HKLException);

  CPPUNIT_ASSERT_NO_THROW(s.setWaveLength(1.));
  CPPUNIT_ASSERT_EQUAL(Value(1.), s.get_waveLength());

}

void
sourceTest::SetDirection(void)
{
  Source s(1., &_v);
  hkl_svector axe = {{1, 1, 0}};
  hkl_svector axe_ref = {{1/sqrt(2.), 1/sqrt(2.), 0}};

  hkl_svector null = {{0, 0, 0}};
  CPPUNIT_ASSERT_THROW(s.setDirection(&null), HKLException);
  CPPUNIT_ASSERT_NO_THROW(s.setDirection(&axe));

  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&axe_ref, s.get_direction()));
}

void
sourceTest::GetSetKi(void)
{
  Source s(1.54, &_v);

  hkl_svector ki;
  hkl_svector ki_ref;

  ki_ref = _v;
  ::hkl_svector_times_double(&ki_ref, HKL_TAU / 1.54);
  s.get_ki(&ki);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&ki_ref, &ki));

  hkl_svector null = {{0, 0, 0}};
  CPPUNIT_ASSERT_THROW(s.set_ki(&null), HKLException);

  ki_ref.data[X] = 1;
  ki_ref.data[Y] = 1;
  ki_ref.data[Z] = 0;
  CPPUNIT_ASSERT_NO_THROW(s.set_ki(&ki_ref));
  s.get_ki(&ki);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&ki_ref, &ki));
}
