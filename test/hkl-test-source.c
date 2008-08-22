#include <math.h>

#include <hkl/hkl-source.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME source

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklSource s, c;

	hkl_source_init(&s, 1.54, 1, 0, 0);
	c = s;

	HKL_ASSERT_DOUBLES_EQUAL(c.wave_length, s.wave_length, HKL_EPSILON);
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_vector_cmp(&c.direction, &s.direction));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(init)
{
	HklSource s;

	hkl_source_init(&s, 1, 1, 0, 0);

	HKL_ASSERT_DOUBLES_EQUAL(1., s.wave_length, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., s.direction.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., s.direction.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., s.direction.data[2], HKL_EPSILON);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklSource ref, s1, s2;

	hkl_source_init(&ref, 1.54, 1, 0, 0);
	hkl_source_init(&s1, 1.54, 1, 0, 0);
	hkl_source_init(&s2, 1, 1, 0, 0);

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(&ref, &s1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_source_cmp(&ref, &s2));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(compute_ki)
{
	HklSource s;
	HklVector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
	HklVector ki;

	hkl_source_init(&s, 1.54, 1, 0, 0);

	hkl_source_compute_ki(&s, &ki);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&ki_ref, &ki));

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new_copy );
HKL_TEST( init );
HKL_TEST( cmp );
HKL_TEST( compute_ki );

HKL_TEST_SUITE_END
