#include <stdio.h>
#include "test.h"

int main(int argc, char **argv)
{
	int res;

	struct hkl_tests tests;

	hkl_tests_init(&tests, 0);

	HKL_TEST_SUITE(svector);
	HKL_TEST_SUITE(smatrix);
	HKL_TEST_SUITE(quaternion);
	HKL_TEST_SUITE(interval);
	HKL_TEST_SUITE(source);
	
	res = hkl_tests_run(&tests);
	hkl_tests_release(&tests);

	return res;
}
