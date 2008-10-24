#include <stdlib.h>
#include <hkl/hkl-list.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME list

static void *copy(void const *src)
{
	double *d_copy = malloc(sizeof(double));
	double const *d_src = src;
	*d_copy = *d_src;
	return d_copy;
}

HKL_TEST_SUITE_FUNC(new)
{
	HklList *list;

	list = hkl_list_new();

	HKL_ASSERT_EQUAL(0, list->len);
	HKL_ASSERT_EQUAL(0, list->alloc);
	HKL_ASSERT_POINTER_EQUAL(NULL, list->data);

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(append)
{
	HklList *list;
	unsigned int i;

	list = hkl_list_new_managed(&copy, &free);

	HKL_ASSERT_EQUAL(0, list->len);
	HKL_ASSERT_EQUAL(0, list->alloc);
	HKL_ASSERT_POINTER_EQUAL(NULL, list->data);

	for(i=0; i<10; i++) {
		hkl_list_append(list, malloc(sizeof(double)));
		HKL_ASSERT_EQUAL(i+1, list->len);
		HKL_ASSERT_EQUAL(0, !list->data[i]);
	}


	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklList *list1;
	HklList *list2;
	unsigned int i;

	list1 = hkl_list_new_managed(&copy, &free);

	HKL_ASSERT_EQUAL(0, list1->len);
	HKL_ASSERT_EQUAL(0, list1->alloc);
	HKL_ASSERT_POINTER_EQUAL(NULL, list1->data);

	for(i=0; i<10; i++) {
		double *d = malloc(sizeof(double));

		*d = i;
		hkl_list_append(list1, d);
	}

	list2 = hkl_list_new_copy(list1);
	for(i=0; i<10; i++) {
		double *d1 = list1->data[i];
		double *d2 = list2->data[i];
		HKL_ASSERT_DOUBLES_EQUAL(*d1, *d2, HKL_EPSILON);
	}


	hkl_list_free(list2);
	hkl_list_free(list1);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_by_idx)
{
	HklList *list;
	unsigned int i;
	double *dd;

	list = hkl_list_new_managed(&copy, &free);

	for(i=0; i<10; i++) {
		double *d = malloc(sizeof(double));

		*d = i;
		hkl_list_append(list, d);
	}

	
	for(i=0; i<10; i++) {
		dd = hkl_list_get_by_idx(list, i);
		HKL_ASSERT_DOUBLES_EQUAL((double)i, *dd, HKL_EPSILON);
	}


	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(del_by_idx)
{
	HklList *list;
	unsigned int i;

	list = hkl_list_new_managed(&copy, &free);

	HKL_ASSERT_EQUAL(0, list->len);
	HKL_ASSERT_EQUAL(0, list->alloc);
	HKL_ASSERT_POINTER_EQUAL(NULL, list->data);

	for(i=0; i<10; i++) {
		hkl_list_append(list, malloc(sizeof(double)));
		HKL_ASSERT_EQUAL(i+1, list->len);
		HKL_ASSERT_EQUAL(0, !list->data);
	}
	for(i=0; i<10; i++) {
		hkl_list_del_by_idx(list, 0);
		HKL_ASSERT_EQUAL(10 - (i + 1), list->len);
		HKL_ASSERT_EQUAL(0, !list->data);
	}

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_idx)
{
	HklList *list;
	double *d;

	list = hkl_list_new_managed(&copy, &free);

	d = malloc(sizeof(double));
	hkl_list_append(list, d);
	HKL_ASSERT_EQUAL(-1, hkl_list_get_idx(list, NULL));
	HKL_ASSERT_EQUAL(0, hkl_list_get_idx(list, d));

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(foreach)
{
	HklList *list;
	double *d;
	unsigned int i;

	list = hkl_list_new_managed(&copy, &free);

	
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
		d = list->data[i];
		HKL_ASSERT_DOUBLES_EQUAL(i + 1., *d, HKL_EPSILON);
	}

	hkl_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( append );
HKL_TEST( new_copy );
HKL_TEST( get_by_idx );
HKL_TEST( del_by_idx );
HKL_TEST( get_idx );
HKL_TEST( foreach );

HKL_TEST_SUITE_END
