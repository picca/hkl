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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME lattice

HKL_TEST_SUITE_FUNC(new)
{
	HklLattice *lattice;

	// can not set this lattice
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 10*HKL_DEGTORAD, 120*HKL_DEGTORAD);
	HKL_ASSERT_POINTER_EQUAL(NULL, lattice);

	// but can create this one
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(0, !lattice);

	HKL_ASSERT_DOUBLES_EQUAL(1.54, lattice->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, lattice->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, lattice->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90*HKL_DEGTORAD, lattice->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90*HKL_DEGTORAD, lattice->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90*HKL_DEGTORAD, lattice->gamma->value, HKL_EPSILON);
	hkl_lattice_free(lattice);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( new_copy )
{
	HklLattice *lattice;
	HklLattice *copy;

	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	// copy constructor
	copy = hkl_lattice_new_copy(lattice);

	HKL_ASSERT_DOUBLES_EQUAL(1.54, copy->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, copy->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, copy->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, copy->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, copy->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, copy->gamma->value, HKL_EPSILON);

	hkl_lattice_free(lattice);
	hkl_lattice_free(copy);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set)
{
	HklLattice *lattice;

	// can not set this lattice
	lattice = hkl_lattice_new_default();

	// but can create this one
	hkl_lattice_set(lattice, 1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 92*HKL_DEGTORAD);

	HKL_ASSERT_DOUBLES_EQUAL(1.54, lattice->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, lattice->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, lattice->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90*HKL_DEGTORAD, lattice->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(91*HKL_DEGTORAD, lattice->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(92*HKL_DEGTORAD, lattice->gamma->value, HKL_EPSILON);
	hkl_lattice_free(lattice);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( reciprocal )
{
	HklLattice *lattice;
	HklLattice *reciprocal;

	lattice = hkl_lattice_new_default();
	reciprocal = hkl_lattice_new_default();

	// cubic
	hkl_lattice_set(lattice, 1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_lattice_reciprocal(lattice, reciprocal));

	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 1.54, reciprocal->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 1.54, reciprocal->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 1.54, reciprocal->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->gamma->value, HKL_EPSILON);

	//orthorombic
	hkl_lattice_set(lattice, 1., 3., 4., 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_lattice_reciprocal(lattice, reciprocal));

	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 1., reciprocal->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 3., reciprocal->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 4., reciprocal->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->gamma->value, HKL_EPSILON);

	// hexagonal1
	hkl_lattice_set(lattice, 1., 2., 1., 90 * HKL_DEGTORAD, 120 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_lattice_reciprocal(lattice, reciprocal));

	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 2. / sqrt(3.), reciprocal->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 2., reciprocal->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 2. / sqrt(3.), reciprocal->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(60. * HKL_DEGTORAD, reciprocal->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->gamma->value, HKL_EPSILON);

	// hexagonal2
	hkl_lattice_set(lattice, 2., 1., 1., 120 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_lattice_reciprocal(lattice, reciprocal));

	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU / 2., reciprocal->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 2. / sqrt(3.), reciprocal->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 2. / sqrt(3.), reciprocal->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(60. * HKL_DEGTORAD, reciprocal->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90. * HKL_DEGTORAD, reciprocal->gamma->value, HKL_EPSILON);

	// triclinic1
	hkl_lattice_set(lattice, 9.32, 8.24, 13.78, 91.23 * HKL_DEGTORAD, 93.64 * HKL_DEGTORAD, 122.21 * HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_lattice_reciprocal(lattice, reciprocal));

	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 0.1273130168, reciprocal->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 0.1437422974, reciprocal->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 0.0728721120, reciprocal->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.5052513337, reciprocal->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.482101482, reciprocal->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.0055896011, reciprocal->gamma->value, HKL_EPSILON);

	// triclinic2
	hkl_lattice_set(lattice, 18.423, 18.417, 18.457, 89.99 * HKL_DEGTORAD, 89.963 * HKL_DEGTORAD, 119.99 * HKL_DEGTORAD);
	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_lattice_reciprocal(lattice, reciprocal));

	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 0.0626708259, reciprocal->a->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 0.0626912310, reciprocal->b->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(HKL_TAU * 0.0541800061, reciprocal->c->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.5713705262, reciprocal->alpha->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.5716426508, reciprocal->beta->value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.0473718249, reciprocal->gamma->value, HKL_EPSILON);

	hkl_lattice_free(lattice);
	hkl_lattice_free(reciprocal);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( get_B )
{
	static HklMatrix B_ref = {{{HKL_TAU / 1.54,              0,              0},
		{             0, HKL_TAU / 1.54,              0},
		{             0,              0, HKL_TAU / 1.54}}};
	HklLattice *lattice;
	HklMatrix B;

	// cubic
	lattice = hkl_lattice_new(1.54, 1.54, 1.54, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);

	hkl_lattice_get_B(lattice, &B);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&B_ref, &B));

	hkl_lattice_free(lattice);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( new_copy );
HKL_TEST( set );
HKL_TEST( reciprocal );
HKL_TEST( get_B );

HKL_TEST_SUITE_END
