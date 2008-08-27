#include <stdio.h>

#include "hkl-test.h"

int main(int argc, char **argv)
{
	int res;

	struct hkl_tests tests;

	hkl_tests_init(&tests, 0);

	HKL_TEST_SUITE( vector );
	HKL_TEST_SUITE( smatrix );
	HKL_TEST_SUITE( quaternion );
	//HKL_TEST_SUITE( interval );
	HKL_TEST_SUITE( source );
	HKL_TEST_SUITE( axis );
	HKL_TEST_SUITE( holder );
	HKL_TEST_SUITE( detector );
	HKL_TEST_SUITE( geometry );
	HKL_TEST_SUITE( parameter );
	HKL_TEST_SUITE( lattice );
	HKL_TEST_SUITE( list );
	HKL_TEST_SUITE( sample );
	HKL_TEST_SUITE( pseudoaxis_E4CV );
	HKL_TEST_SUITE( pseudoaxis_K4CV );

	res = hkl_tests_run(&tests);
	hkl_tests_release(&tests);

	return res;
}
