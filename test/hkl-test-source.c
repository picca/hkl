#include <math.h>

#include <hkl/hkl-source.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME source

HKL_TEST_SUITE_FUNC(new)
{
	HklSource *s;

	s = hkl_source_new(1.54, 1, 0, 0);
	
	HKL_ASSERT_EQUAL(1.54, s->wave_length);
	HKL_ASSERT_EQUAL(1., s->direction->data[0]);
	HKL_ASSERT_EQUAL(0., s->direction->data[1]);
	HKL_ASSERT_EQUAL(0., s->direction->data[2]);

	hkl_source_free(s);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklSource *s, *copy;

	s = hkl_source_new(1.54, 1, 0, 0);
	copy = hkl_source_new_copy(s);
	
	HKL_ASSERT_EQUAL(copy->wave_length, s->wave_length);
	HKL_ASSERT_EQUAL(copy->direction->data[0], s->direction->data[0]);
	HKL_ASSERT_EQUAL(copy->direction->data[1], s->direction->data[1]);
	HKL_ASSERT_EQUAL(copy->direction->data[2], s->direction->data[2]);

	hkl_source_free(s);
	hkl_source_free(copy);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklSource *s;

	s = hkl_source_new(1.54, 1, 0, 0);
	hkl_source_set(s, 1, 1, 0, 0);
	
	HKL_ASSERT_EQUAL(1., s->wave_length);
	HKL_ASSERT_EQUAL(1., s->direction->data[0]);
	HKL_ASSERT_EQUAL(0., s->direction->data[1]);
	HKL_ASSERT_EQUAL(0., s->direction->data[2]);

	hkl_source_free(s);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(cmp)
{
	HklSource *ref, *s1, *s2;

	ref = hkl_source_new(1.54, 1, 0, 0);
	s1 = hkl_source_new(1.54, 1, 0, 0);
	s2 = hkl_source_new(1, 1, 0, 0);

	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_source_cmp(ref, s1));
	HKL_ASSERT_EQUAL(HKL_FALSE, hkl_source_cmp(ref, s2));

	hkl_source_free(ref);
	hkl_source_free(s1);
	hkl_source_free(s2);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_ki)
{
	HklSource *s;
	HklVector ki_ref = {{HKL_TAU / 1.54, 0, 0}};
	HklVector ki;

	s = hkl_source_new(1.54, 1, 0, 0);

	hkl_source_get_ki(s, &ki);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_vector_cmp(&ki_ref, &ki));

	hkl_source_free(s);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( new_copy );
HKL_TEST( set );
HKL_TEST( cmp );
HKL_TEST( get_ki );

HKL_TEST_SUITE_END
