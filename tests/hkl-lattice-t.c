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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>

static void new(void)
{
	HklLattice *lattice;

	/* can not set this lattice */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 10*HKL_DEGTORAD, 120*HKL_DEGTORAD);
	ok(NULL == lattice, __func__);

	/* but can create this one */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	ok(0 == !lattice, __func__);

	is_double(1.54, hkl_parameter_value_get(hkl_lattice_a_get(lattice)), HKL_EPSILON, __func__);
	is_double(1.54, hkl_parameter_value_get(hkl_lattice_b_get(lattice)), HKL_EPSILON, __func__);
	is_double(1.54, hkl_parameter_value_get(hkl_lattice_c_get(lattice)), HKL_EPSILON, __func__);
	is_double(90*HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(lattice)), HKL_EPSILON, __func__);
	is_double(90*HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(lattice)), HKL_EPSILON, __func__);
	is_double(90*HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(lattice)), HKL_EPSILON, __func__);
	hkl_lattice_free(lattice);
}

static void  new_copy(void )
{
	HklLattice *lattice;
	HklLattice *copy;

	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	/* copy constructor */
	copy = hkl_lattice_new_copy(lattice);

	is_double(1.54, hkl_parameter_value_get(hkl_lattice_a_get(copy)), HKL_EPSILON, __func__);
	is_double(1.54, hkl_parameter_value_get(hkl_lattice_b_get(copy)), HKL_EPSILON, __func__);
	is_double(1.54, hkl_parameter_value_get(hkl_lattice_c_get(copy)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(copy)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(copy)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(copy)), HKL_EPSILON, __func__);

	hkl_lattice_free(lattice);
	hkl_lattice_free(copy);
}

static void set(void)
{
	HklLattice *lattice;

	/* can not set this lattice */
	lattice = hkl_lattice_new_default();

	/* but can create this one */
	hkl_lattice_set(lattice, 1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 91*HKL_DEGTORAD, 92*HKL_DEGTORAD);

	is_double(1.54, hkl_parameter_value_get(hkl_lattice_a_get(lattice)), HKL_EPSILON, __func__);
	is_double(1.54, hkl_parameter_value_get(hkl_lattice_b_get(lattice)), HKL_EPSILON, __func__);
	is_double(1.54, hkl_parameter_value_get(hkl_lattice_c_get(lattice)), HKL_EPSILON, __func__);
	is_double(90*HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(lattice)), HKL_EPSILON, __func__);
	is_double(91*HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(lattice)), HKL_EPSILON, __func__);
	is_double(92*HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(lattice)), HKL_EPSILON, __func__);
	hkl_lattice_free(lattice);
}

static void  reciprocal(void )
{
	HklLattice *lattice;
	HklLattice *reciprocal;

	lattice = hkl_lattice_new_default();
	reciprocal = hkl_lattice_new_default();

	/* cubic */
	hkl_lattice_set(lattice, 1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);

	ok(HKL_TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	is_double(HKL_TAU / 1.54, hkl_parameter_value_get(hkl_lattice_a_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU / 1.54, hkl_parameter_value_get(hkl_lattice_b_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU / 1.54, hkl_parameter_value_get(hkl_lattice_c_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(reciprocal)), HKL_EPSILON, __func__);

	/* orthorombic */
	hkl_lattice_set(lattice, 1., 3., 4., 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);
	ok(HKL_TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	is_double(HKL_TAU / 1., hkl_parameter_value_get(hkl_lattice_a_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU / 3., hkl_parameter_value_get(hkl_lattice_b_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU / 4., hkl_parameter_value_get(hkl_lattice_c_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(reciprocal)), HKL_EPSILON, __func__);

	/* hexagonal1 */
	hkl_lattice_set(lattice, 1., 2., 1., 90 * HKL_DEGTORAD, 120 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);
	ok(HKL_TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	is_double(HKL_TAU * 2. / sqrt(3.), hkl_parameter_value_get(hkl_lattice_a_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU / 2., hkl_parameter_value_get(hkl_lattice_b_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 2. / sqrt(3.), hkl_parameter_value_get(hkl_lattice_c_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(60. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(reciprocal)), HKL_EPSILON, __func__);

	/* hexagonal2 */
	hkl_lattice_set(lattice, 2., 1., 1., 120 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);
	ok(HKL_TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	is_double(HKL_TAU / 2., hkl_parameter_value_get(hkl_lattice_a_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 2. / sqrt(3.), hkl_parameter_value_get(hkl_lattice_b_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 2. / sqrt(3.), hkl_parameter_value_get(hkl_lattice_c_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(60. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_alpha_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_beta_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(90. * HKL_DEGTORAD, hkl_parameter_value_get(hkl_lattice_gamma_get(reciprocal)), HKL_EPSILON, __func__);

	/* triclinic1 */
	hkl_lattice_set(lattice, 9.32, 8.24, 13.78, 91.23 * HKL_DEGTORAD, 93.64 * HKL_DEGTORAD, 122.21 * HKL_DEGTORAD);
	ok(HKL_TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	is_double(HKL_TAU * 0.1273130168, hkl_parameter_value_get(hkl_lattice_a_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 0.1437422974, hkl_parameter_value_get(hkl_lattice_b_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 0.0728721120, hkl_parameter_value_get(hkl_lattice_c_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(1.5052513337, hkl_parameter_value_get(hkl_lattice_alpha_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(1.482101482, hkl_parameter_value_get(hkl_lattice_beta_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(1.0055896011, hkl_parameter_value_get(hkl_lattice_gamma_get(reciprocal)), HKL_EPSILON, __func__);

	/* triclinic2 */
	hkl_lattice_set(lattice, 18.423, 18.417, 18.457, 89.99 * HKL_DEGTORAD, 89.963 * HKL_DEGTORAD, 119.99 * HKL_DEGTORAD);
	ok(HKL_TRUE == hkl_lattice_reciprocal(lattice, reciprocal), __func__);

	is_double(HKL_TAU * 0.0626708259, hkl_parameter_value_get(hkl_lattice_a_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 0.0626912310, hkl_parameter_value_get(hkl_lattice_b_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(HKL_TAU * 0.0541800061, hkl_parameter_value_get(hkl_lattice_c_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(1.5713705262, hkl_parameter_value_get(hkl_lattice_alpha_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(1.5716426508, hkl_parameter_value_get(hkl_lattice_beta_get(reciprocal)), HKL_EPSILON, __func__);
	is_double(1.0473718249, hkl_parameter_value_get(hkl_lattice_gamma_get(reciprocal)), HKL_EPSILON, __func__);

	hkl_lattice_free(lattice);
	hkl_lattice_free(reciprocal);
}

static void  get_B(void )
{
	HklMatrix *B_ref = hkl_matrix_new_full(HKL_TAU / 1.54, 0, 0,
					       0, HKL_TAU / 1.54, 0,
					       0, 0, HKL_TAU / 1.54);
	HklLattice *lattice;
	HklMatrix *B = hkl_matrix_new();

	/* cubic */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);

	hkl_lattice_get_B(lattice, B);
	ok(HKL_TRUE == hkl_matrix_cmp(B_ref, B), __func__);

	hkl_lattice_free(lattice);
	hkl_matrix_free(B);
	hkl_matrix_free(B_ref);
}

static void  get_1_B(void )
{
	HklMatrix *I_ref = hkl_matrix_new_full(1, 0, 0,
					       0, 1, 0,
					       0, 0, 1);
	HklLattice *lattice;
	HklMatrix *I = hkl_matrix_new();
	HklMatrix *B_1 = hkl_matrix_new();

	/* cubic */
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD, 90 * HKL_DEGTORAD);

	hkl_lattice_get_B(lattice, I);
	hkl_lattice_get_1_B(lattice, B_1);

	/* B times B^-1 = Identity */
	hkl_matrix_times_matrix(I, B_1);
	ok(HKL_TRUE == hkl_matrix_cmp(I_ref, I), __func__);

	hkl_lattice_free(lattice);
	hkl_matrix_free(B_1);
	hkl_matrix_free(I);
	hkl_matrix_free(I_ref);
}

int main(int argc, char** argv)
{
	plan(64);

	new();
	new_copy();
	set();
	reciprocal();
	get_B();
	get_1_B();

	return 0;
}
