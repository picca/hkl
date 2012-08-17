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
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <ccan/array_size/array_size.h>
#include <hkl/hkl-pseudoaxis-zaxis.h>
#include <hkl/hkl-pseudoaxis-private.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/* #define DEBUG */

/***********************/
/* numerical functions */
/***********************/

static int _reflectivity_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double gamma = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = mu - gamma;

	return  GSL_SUCCESS;
}

static const HklFunction reflectivity_func = {
	.function = _reflectivity_func,
	.size = 4,
};

/********/
/* mode */
/********/

static HklPseudoAxisEngineMode* zaxis()
{
	static const char* axes[] = {"omega", "delta", "gamma"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations);
}

static HklPseudoAxisEngineMode* reflectivity()
{
	static const char* axes[] = {"mu", "omega", "delta", "gamma"};
	static const HklFunction *functions[] = {&reflectivity_func};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations);
}

/**********************/
/* pseudo axis engine */
/**********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_zaxis_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *default_mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	default_mode = zaxis();
	hkl_pseudo_axis_engine_add_mode(self, default_mode);
	hkl_pseudo_axis_engine_select_mode(self, default_mode);

	hkl_pseudo_axis_engine_add_mode(self, reflectivity());

	return self;
}
