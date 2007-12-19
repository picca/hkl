#include <stdlib.h>
#include <stdio.h>

#include <hkl/hkl-macros.h>

#include "test.h"

void hkl_tests_grow(struct hkl_tests * tests, size_t extra)
{
	if (tests->len + extra <= tests->len)
		die("you want to use way too much memory");
	if (!tests->alloc)
		tests->tests = NULL;
	ALLOC_GROW(tests->tests, tests->len + extra, tests->alloc);
}

void hkl_tests_init(struct hkl_tests *tests, size_t hint)
{
	tests->alloc = tests->len = 0;
	if (hint)
		hkl_tests_grow(tests, hint);
	else
		tests->tests = NULL;
}

void hkl_tests_release(struct hkl_tests *tests)
{
	if (tests->alloc) {
		free(tests->tests);
		hkl_tests_init(tests, 0);
	}
}

void hkl_tests_add_test(struct hkl_tests *tests, const char *name, hkl_test_method method)
{
	struct hkl_test *test;

	hkl_tests_grow(tests, 1);

	test = &tests->tests[tests->len];
	test->name = name;
	test->method = method;

	tests->len++;
}

int hkl_test_run(struct hkl_test * test)
{
	return (*(test->method))(test);
}

int hkl_tests_run(struct hkl_tests * tests)
{
	size_t i;
	int res = 0;

	for(i=0; i<tests->len; i++) {
		struct hkl_test *test = &tests->tests[i];
		if (!hkl_test_run(test)) {
			printf("\n%s:%d: FAIL %s\n", test->file, test->line, test->name);
			exit(-1);
			res = -1;
			break;
		} else
			printf(".");
	}
	printf("\n");
	return res;
}
