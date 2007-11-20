#ifndef _TEST_H_
#define _TEST_H_

#include <stdio.h>
#include <math.h>

#define HKL_TEST_PASS 1
#define HKL_TEST_FAIL 0

#define HKL_ASSERT_EQUAL(a, b) do {\
	test->file = __FILE__;\
	test->line = __LINE__;\
	if (a == b)\
		return HKL_TEST_PASS;\
	else\
		return HKL_TEST_FAIL;\
} while(0)

#define HKL_ASSERT_DOUBLES_EQUAL(a, b, c) do {\
	test->file = __FILE__;\
	test->line = __LINE__;\
	if (fabs(a-b) <= c)\
		return HKL_TEST_PASS;\
	else\
		return HKL_TEST_FAIL;\
} while(0)

#define HKL_TEST_SUITE_FUNC(a, b) int hkl_test_ ## a ## _ ## b(struct hkl_test *test)

#define HKL_TEST_SUITE(a) hkl_test_suite_ ## a(&tests)

#define HKL_TEST_SUITE_BEGIN(a) void hkl_test_suite_ ## a(struct hkl_tests *tests) {

#define HKL_TEST_SUITE_ADD_FUNC(a, b) do {\
	hkl_tests_add_test(tests, "hkl_test_" #a "_" #b, &(hkl_test_ ## a ## _ ## b) );\
} while(0)

#define HKL_TEST_SUITE_END }

/* forward declaration for the hkl_test_method typedef */
struct hkl_test;

typedef int (*hkl_test_method)(struct hkl_test *test);

struct hkl_test {
	const char *name;
	hkl_test_method method;
	const char *file;
	int line;
};

struct hkl_tests {
	size_t alloc;
	size_t len;
	struct hkl_test *tests;
};


extern void hkl_tests_init(struct hkl_tests * tests, size_t hint);

extern void hkl_tests_add_test(struct hkl_tests *tests, const char *name, hkl_test_method method);

extern int hkl_tests_run(struct hkl_tests *tests);

extern void hkl_tests_release(struct hkl_tests *tests);

#endif // _TEST_H_
