#include <cmath>

#include "config.h"
#include "source_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( sourceTest );

void sourceTest::setUp(void) {;}

void sourceTest::tearDown(void) {;}

void
sourceTest::hkl_source_cmp(void)
{
  static hkl_source source_ref = {1.54, {{1, 0, 0}}};
  hkl_source source1 = {1.54, {{1, 0, 0}}};
  hkl_source source2 = {1, {{1, 0, 0}}};

  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_source_cmp(&source_ref, &source1));
  CPPUNIT_ASSERT_EQUAL(HKL_FALSE, ::hkl_source_cmp(&source_ref, &source2));

  // test assignation
  source2 = source_ref;
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_source_cmp(&source_ref, &source2));
}

void
sourceTest::hkl_source_get_ki(void)
{
  static hkl_source source_ref = {1.54, {{1, 0, 0}}};
  static hkl_svector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
  hkl_svector ki;

  ::hkl_source_get_ki(&source_ref, &ki);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&ki_ref, &ki));
}
