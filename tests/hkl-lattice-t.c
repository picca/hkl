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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>

#define CHECK_PARAM(_lattice, _param, _value)				\
	is_double(_value,						\
		  hkl_parameter_value_get(hkl_lattice_## _param ##_get(_lattice), \
					  HKL_UNIT_DEFAULT),		\
		  HKL_EPSILON, __func__);

#define CHECK_LATTICE(_lattice, _a, _b, _c, _alpha, _beta, _gamma) do {	\
		CHECK_PARAM(_lattice, a, _a);				\
		CHECK_PARAM(_lattice, b, _b);				\
		CHECK_PARAM(_lattice, c, _c);				\
		CHECK_PARAM(_lattice, alpha, _alpha);			\
		CHECK_PARAM(_lattice, beta, _beta);			\
		CHECK_PARAM(_lattice, gamma, _gamma);			\
	}while(0)

#define SET_PARAM(_lattice, _param, _value) do {			\
		GError *error;						\
		HklParameter *p = hkl_parameter_new_copy(hkl_lattice_ ## _param ## _get(_lattice)); \
		hkl_parameter_value_set(p, _value, HKL_UNIT_DEFAULT, NULL); \
		ok(TRUE == hkl_lattice_ ## _param ## _set(_lattice, p, NULL), __func__); \
		error = NULL;						\
		ok(TRUE == hkl_lattice_ ## _param ## _set(_lattice, p, &error), __func__); \
		ok(error == NULL, __func__);				\
		hkl_parameter_free(p);					\
	}while(0)
				\
#define SET_LATTICE(_lattice, _a, _b, _c, _alpha, _beta, _gamma) do{	\
		SET_PARAM(_lattice, a, _a);				\
		SET_PARAM(_lattice, b, _b);				\
		SET_PARAM(_lattice, c, _c);				\
		SET_PARAM(_lattice, alpha, _alpha);			\
		SET_PARAM(_lattice, beta, _beta);			\
		SET_PARAM(_lattice, gamma, _gamma);			\
	}while(0)

static void new(void)
{
	HklLattice *lattice;
	GError *error;

	/* can not set this lattice */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 10*HKL_DEGTORAD, 120*HKL_DEGTORAD,
				  NULL);
	ok(NULL == lattice, __func__);

	/* check GError generation */
	error = NULL;
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 10*HKL_DEGTORAD, 120*HKL_DEGTORAD,
				  &error);
	ok(NULL == lattice, __func__);
	ok(error != NULL, __func__);
	g_clear_error(&error);

	/* but can create this one */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				  NULL);
	ok(0 == !lattice, __func__);

	CHECK_LATTICE(lattice,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	/* but can create this one and no GError are produce */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				  &error);
	ok(0 == !lattice, __func__);
	ok(error == NULL, __func__);

	CHECK_LATTICE(lattice,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	hkl_lattice_free(lattice);
}

static void new_copy(void)
{
	HklLattice *lattice;
	HklLattice *copy;

	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				  NULL);

	/* copy constructor */
	copy = hkl_lattice_new_copy(lattice);

	CHECK_LATTICE(copy,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	hkl_lattice_free(lattice);
	hkl_lattice_free(copy);
}

static void set(void)
{
	HklLattice *lattice;
	GError *error;

	lattice = hkl_lattice_new_default();

	/* can not set this lattice */
	ok(FALSE == hkl_lattice_set(lattice,
				    1.54, 1.54, 1.54,
				    90*HKL_DEGTORAD, 10*HKL_DEGTORAD, 120*HKL_DEGTORAD,
				    HKL_UNIT_DEFAULT, NULL),
	   __func__);

	/* can not set this lattice with GError */
	error = NULL;
	ok(FALSE == hkl_lattice_set(lattice,
				    1.54, 1.54, 1.54,
				    90*HKL_DEGTORAD, 10*HKL_DEGTORAD, 120*HKL_DEGTORAD,
				    HKL_UNIT_DEFAULT, &error),
	   __func__);
	ok(error != NULL, __func__);
	g_clear_error(&error);

	/* can set this lattice */
	ok(TRUE == hkl_lattice_set(lattice,
				   1.54, 1.54, 1.54,
				   90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 92*HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);
	CHECK_LATTICE(lattice,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 92*HKL_DEGTORAD);

	/* can set this lattice with no GError set */
	ok(TRUE == hkl_lattice_set(lattice,
				   1.54, 1.54, 1.54,
				   90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, &error),
	   __func__);
	ok(error == NULL, __func__);
	CHECK_LATTICE(lattice,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 90*HKL_DEGTORAD);



	/* can set this lattice in HKL_UNIT_USER with no GError set */
	ok(TRUE == hkl_lattice_set(lattice, 1.54, 1.54, 1.54, 90, 91, 92,
				   HKL_UNIT_USER, NULL),
	   __func__);
	CHECK_LATTICE(lattice,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 92*HKL_DEGTORAD);

	/* can set this lattice in HKL_UNIT_USER with no GError set */
	ok(TRUE == hkl_lattice_set(lattice,
				   1.54, 1.54, 1.54, 90, 91, 90,
				   HKL_UNIT_USER, &error),
	   __func__);
	ok(error == NULL, __func__);
	CHECK_LATTICE(lattice,
		      1.54, 1.54, 1.54,
		      90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	/* check individual accessor */
	SET_LATTICE(lattice, 1.54, 1.54, 1.54, 90*HKL_DEGTORAD, 91 * HKL_DEGTORAD, 91 * HKL_DEGTORAD);
	CHECK_LATTICE(lattice, 1.54, 1.54, 1.54, 90*HKL_DEGTORAD, 91 * HKL_DEGTORAD, 91 * HKL_DEGTORAD);

	hkl_lattice_free(lattice);
}

static void reciprocal(void)
{
	HklLattice *lattice;
	HklLattice *reciprocal;

	lattice = hkl_lattice_new_default();
	reciprocal = hkl_lattice_new_default();

	/* cubic */
	ok(TRUE == hkl_lattice_set(lattice, 1.54, 1.54, 1.54,
				   90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);

	ok(TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	CHECK_LATTICE(reciprocal,
		      HKL_TAU / 1.54, HKL_TAU / 1.54, HKL_TAU / 1.54,
		      90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	/* orthorombic */
	ok(TRUE == hkl_lattice_set(lattice,
				   1., 3., 4.,
				   90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);
	ok(TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	CHECK_LATTICE(reciprocal,
		      HKL_TAU / 1., HKL_TAU / 3., HKL_TAU / 4.,
		      90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	/* hexagonal1 */
	ok(TRUE == hkl_lattice_set(lattice,
				   1., 2., 1.,
				   90 * HKL_DEGTORAD, 120 * HKL_DEGTORAD, 90 * HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);
	ok(TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	CHECK_LATTICE(reciprocal,
		      HKL_TAU * 2. / sqrt(3.), HKL_TAU / 2., HKL_TAU * 2. / sqrt(3.),
		      90. * HKL_DEGTORAD, 60. * HKL_DEGTORAD, 90. * HKL_DEGTORAD);

	/* hexagonal2 */
	ok(TRUE == hkl_lattice_set(lattice, 2., 1., 1.,
				   120 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);
	ok(TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	CHECK_LATTICE(reciprocal,
		      HKL_TAU / 2., HKL_TAU * 2. / sqrt(3.), HKL_TAU * 2. / sqrt(3.),
		      60. * HKL_DEGTORAD, 90. * HKL_DEGTORAD, 90. * HKL_DEGTORAD);

	/* triclinic1 */
	ok(TRUE == hkl_lattice_set(lattice, 9.32, 8.24, 13.78,
				   91.23 * HKL_DEGTORAD, 93.64 * HKL_DEGTORAD, 122.21 * HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);
	ok(TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	CHECK_LATTICE(reciprocal,
		      HKL_TAU * 0.1273130168,HKL_TAU * 0.1437422974,HKL_TAU * 0.0728721120,
		      1.5052513337, 1.482101482, 1.0055896011);

	/* triclinic2 */
	ok(TRUE == hkl_lattice_set(lattice, 18.423, 18.417,
				   18.457, 89.99 * HKL_DEGTORAD, 89.963 * HKL_DEGTORAD, 119.99 * HKL_DEGTORAD,
				   HKL_UNIT_DEFAULT, NULL),
	   __func__);
	ok(TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	CHECK_LATTICE(reciprocal,
		      HKL_TAU * 0.0626708259,HKL_TAU * 0.0626912310,HKL_TAU * 0.0541800061,
		      1.5713705262, 1.5716426508, 1.0473718249);

	hkl_lattice_free(lattice);
	hkl_lattice_free(reciprocal);
}

static void get_B(void)
{
	HklMatrix *B_ref = hkl_matrix_new_full(HKL_TAU / 1.54, 0, 0,
					       0, HKL_TAU / 1.54, 0,
					       0, 0, HKL_TAU / 1.54);
	HklLattice *lattice;
	HklMatrix *B = hkl_matrix_new();

	/* cubic */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD,
				  NULL);

	hkl_lattice_get_B(lattice, B);
	ok(TRUE == hkl_matrix_cmp(B_ref, B), __func__);

	hkl_lattice_free(lattice);
	hkl_matrix_free(B);
	hkl_matrix_free(B_ref);
}

static void get_1_B(void)
{
	HklMatrix *I_ref = hkl_matrix_new_full(1, 0, 0,
					       0, 1, 0,
					       0, 0, 1);
	HklLattice *lattice;
	HklMatrix *I = hkl_matrix_new();
	HklMatrix *B_1 = hkl_matrix_new();

	/* cubic */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD,
				  NULL);

	hkl_lattice_get_B(lattice, I);
	hkl_lattice_get_1_B(lattice, B_1);

	/* B times B^-1 = Identity */
	hkl_matrix_times_matrix(I, B_1);
	ok(TRUE == hkl_matrix_cmp(I_ref, I), __func__);

	hkl_lattice_free(lattice);
	hkl_matrix_free(B_1);
	hkl_matrix_free(I);
	hkl_matrix_free(I_ref);
}

int main(int argc, char** argv)
{
	plan(131);

	new();
	new_copy();
	set();
	reciprocal();
	get_B();
	get_1_B();

	return 0;
}
