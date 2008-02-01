#include <math.h>

#include <hkl/hkl-parameter.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME parameter

HKL_TEST_SUITE_FUNC(new)
{
	HklParameter *p;

	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(NULL, hkl_parameter_new("toto", 2, 1, 3, HKL_TRUE));

	p = hkl_parameter_new("toto", 1, 2, 3, HKL_TRUE);
	HKL_ASSERT_EQUAL(0, !p);
	HKL_ASSERT_EQUAL(1., p->range.min);
	HKL_ASSERT_EQUAL(2., p->value);
	HKL_ASSERT_EQUAL(3., p->range.max);
	HKL_ASSERT_EQUAL(HKL_TRUE, p->not_to_fit);

	hkl_parameter_free(p);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklParameter *copy, p;

	hkl_parameter_set(&p, "toto", 1, 2, 3, HKL_TRUE);
	copy = hkl_parameter_new_copy(&p);

	HKL_ASSERT_EQUAL(copy->name, p.name);
	HKL_ASSERT_EQUAL(copy->range.min, p.range.min);
	HKL_ASSERT_EQUAL(copy->value, p.value);
	HKL_ASSERT_EQUAL(copy->range.max, p.range.max);
	HKL_ASSERT_EQUAL(copy->not_to_fit, p.not_to_fit);

	hkl_parameter_free(copy);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklParameter p;

	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_set(&p, "", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_set(&p, "", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_set(&p, "", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_parameter_set(&p, "toto", 2, 1, 3, HKL_TRUE));
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_parameter_set(&p, "toto", 1, 2, 3, HKL_TRUE));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( new_copy );
HKL_TEST( set );

HKL_TEST_SUITE_END
