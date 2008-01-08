#include <math.h>

#include <hkl/hkl-parameter.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME parameter

HKL_TEST_SUITE_FUNC(new)
{
	HklParameter *parameter;

	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("toto", 2, 1, 3, HKL_TRUE));

	parameter = hkl_parameter_new("toto", 1, 2, 3, HKL_TRUE);
	HKL_ASSERT_EQUAL(0, !parameter);
	hkl_parameter_free(parameter);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(init)
{
	HklParameter parameter;

	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&parameter, "", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&parameter, "", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&parameter, "", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_init(&parameter, "toto", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_parameter_init(&parameter, "toto", 1, 2, 3, HKL_TRUE));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( init );

HKL_TEST_SUITE_END
