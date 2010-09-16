/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>

#include "hkl-test.h"

int main(int argc, char **argv)
{
	int res;

	struct hkl_tests tests;

	hkl_tests_init(&tests, 0);

	HKL_TEST_SUITE( error );
	HKL_TEST_SUITE( vector );
	HKL_TEST_SUITE( smatrix );
	HKL_TEST_SUITE( quaternion );
	/* HKL_TEST_SUITE( interval ); */
	HKL_TEST_SUITE( source );
	HKL_TEST_SUITE( axis );
	HKL_TEST_SUITE( detector );
	HKL_TEST_SUITE( geometry );
	HKL_TEST_SUITE( parameter );
	HKL_TEST_SUITE( lattice );
	HKL_TEST_SUITE( sample );
	HKL_TEST_SUITE( pseudoaxis );
	HKL_TEST_SUITE( pseudoaxis_E4CV );
	HKL_TEST_SUITE( pseudoaxis_E6C );
	HKL_TEST_SUITE( pseudoaxis_K4CV );
	HKL_TEST_SUITE( pseudoaxis_K6C );

	res = hkl_tests_run(&tests);
	hkl_tests_release(&tests);

	return res;
}
