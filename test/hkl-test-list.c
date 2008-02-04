#include <hkl/hkl-list.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME list

HKL_TEST_SUITE_FUNC(new)
{
	HklList *list;

	list = hkl_list_new();

	HKL_ASSERT_EQUAL(0, list->len);
	HKL_ASSERT_EQUAL(0, list->alloc);
	HKL_ASSERT_EQUAL(NULL, list->list);

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(append)
{
	HklList *list;
	unsigned int i;

	list = hkl_list_new_managed(&free);

	HKL_ASSERT_EQUAL(0, list->len);
	HKL_ASSERT_EQUAL(0, list->alloc);
	HKL_ASSERT_EQUAL(0, list->list);

	for(i=0; i<10; i++) {
		hkl_list_append(list, malloc(sizeof(double)));
		HKL_ASSERT_EQUAL(i+1, list->len);
		HKL_ASSERT_EQUAL(0, !list->list);
	}


	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(del_by_idx)
{
	HklList *list;
	unsigned int i;

	list = hkl_list_new_managed(&free);

	HKL_ASSERT_EQUAL(0, list->len);
	HKL_ASSERT_EQUAL(0, list->alloc);
	HKL_ASSERT_EQUAL(0, list->list);

	for(i=0; i<10; i++) {
		hkl_list_append(list, malloc(sizeof(double)));
		HKL_ASSERT_EQUAL(i+1, list->len);
		HKL_ASSERT_EQUAL(0, !list->list);
	}
	for(i=0; i<10; i++) {
		hkl_list_del_by_idx(list, 0);
		HKL_ASSERT_EQUAL(10 - (i + 1), list->len);
		HKL_ASSERT_EQUAL(0, !list->list);
	}

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(foreach)
{
	HklList *list;
	double *d;
	unsigned int i;

	list = hkl_list_new_managed(&free);

	
	for(i=0; i<10; i++) {
		d = malloc(sizeof(double));
		*d = i;
		hkl_list_append(list, d);
	}

	void add(void *item)
	{
		double *tmp = item;
		(*tmp)++;
	}
	hkl_list_foreach(list, &add);

	for(i=0; i<10; i++) {
		d = list->list[i];
		HKL_ASSERT_EQUAL(i + 1, *d);
	}

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( append );
HKL_TEST( del_by_idx );
HKL_TEST( foreach );

HKL_TEST_SUITE_END
