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
 * Copyright (C) 2011-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl/hkl-pseudoaxis-soleil-sixs-med.h>
#include <hkl/hkl-pseudoaxis-private.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/**************************/
/* MED2+2 PseudoAxeEngine */
/**************************/

static int reflectivity(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double gamma = x->data[2];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = gamma - 2 * mu;

	return  GSL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_soleil_sixs_med_2_2_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* mu_fixed [default] */
	mode = hkl_pseudo_axis_engine_mode_new(
		"mu_fixed",
		&hkl_full_mode_operations,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "omega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);
	hkl_pseudo_axis_engine_select_mode(self, mode);

	/* reflectivity */
	mode = hkl_pseudo_axis_engine_mode_new(
		"reflectivity",
		&hkl_full_mode_operations,
		1, reflectivity,
		(size_t)0,
		(size_t)4, "mu", "omega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	return self;
}

/**************************/
/* MED1+2 PseudoAxeEngine */
/**************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_soleil_sixs_med_1_2_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* pitch_fixed [default] */
	mode = hkl_pseudo_axis_engine_mode_new(
		"pitch_fixed",
		&hkl_full_mode_operations,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "mu", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);
	hkl_pseudo_axis_engine_select_mode(self, mode);

	return self;
}
