#include <math.h>

#include <hkl/hkl-holders.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME holders

HKL_TEST_SUITE_FUNC(add_holder)
{
	HklHolders *holders = NULL;
	HklHolder *holder = NULL;

	holders = hkl_holders_new();
	HKL_ASSERT_EQUAL(0, holders->holders->len);

	holder = hkl_holders_add_holder(holders);
	hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "B", 1, 0, 0);
	HKL_ASSERT_EQUAL(1, holders->holders->len);

	holder = hkl_holders_add_holder(holders);
	hkl_holder_add_rotation_axis(holder, "A", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "C", 1, 0, 0);
	HKL_ASSERT_EQUAL(2, holders->holders->len);

	hkl_holders_free(holders);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_holder );

HKL_TEST_SUITE_END
