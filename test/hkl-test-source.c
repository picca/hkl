#include <math.h>

#include <hkl/hkl-source.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME source

HKL_TEST_SUITE_FUNC(constructor)
{
	HklSource source_ref = {1.54, {{1, 0, 0}}};
	HklSource *source;
	source = hkl_source_new(1.54, 1, 0, 0);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&source_ref, source));
	hkl_source_free(source);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklSource source_ref = {1.54, {{1, 0, 0}}};
	HklSource source1 = {1.54, {{1, 0, 0}}};
	HklSource source2 = {1, {{1, 0, 0}}};

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&source_ref, &source1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_source_cmp(&source_ref, &source2));

	// test assignation
	source2 = source_ref;
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&source_ref, &source2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_ki)
{
	HklSource source_ref = {1.54, {{1, 0, 0}}};
	HklVector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
	HklVector ki;

	hkl_source_get_ki(&source_ref, &ki);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_svector_cmp(&ki_ref, &ki));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( constructor );
HKL_TEST( cmp );
HKL_TEST( get_ki );

HKL_TEST_SUITE_END
