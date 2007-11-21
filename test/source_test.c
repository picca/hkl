#include <math.h>

#include <hkl/config.h>
#include <hkl/source.h>

#include "test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME source

HKL_TEST_SUITE_FUNC(cmp)
{
  struct hkl_source source_ref = {1.54, {{1, 0, 0}}};
  struct hkl_source source1 = {1.54, {{1, 0, 0}}};
  struct hkl_source source2 = {1, {{1, 0, 0}}};

  HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&source_ref, &source1));
  HKL_ASSERT_EQUAL(HKL_FALSE, hkl_source_cmp(&source_ref, &source2));

  // test assignation
  source2 = source_ref;
  HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&source_ref, &source2));
}

HKL_TEST_SUITE_FUNC(get_ki)
{
  struct hkl_source source_ref = {1.54, {{1, 0, 0}}};
  struct hkl_svector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
  struct hkl_svector ki;

  hkl_source_get_ki(&source_ref, &ki);
  HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&ki_ref, &ki));
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( cmp );
HKL_TEST( get_ki );

HKL_TEST_SUITE_END
