#include <stdio.h>
#include "test.h"
#include "svecmat_test.h"

int main(int argc, char **argv)
{
	int res;

	struct hkl_tests tests;

	hkl_tests_init(&tests, 0);

	HKL_TEST_SUITE(svector);
	
	res = hkl_tests_run(&tests);
	hkl_tests_release(&tests);

	return res;
}
