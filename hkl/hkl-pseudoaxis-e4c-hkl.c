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
 * Copyright (C) 2003-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <ccan/array_size/array_size.h>

#include "hkl-pseudoaxis-auto-private.h"
#include "hkl-pseudoaxis-common-hkl-private.h"

/***********************/
/* numerical functions */
/***********************/

static int _bissector_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double omega = x->data[0];
	const double tth = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_func = {
	.function = _bissector_func,
	.size = 4,
};

/*********/
/* modes */
/*********/

static HklPseudoAxisEngineMode *bissector(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&bissector_func};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_mode_operations);
}

static HklPseudoAxisEngineMode *constant_omega(void)
{
	static const char* axes[] = {"chi", "phi", "tth"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_mode_operations);
}

static HklPseudoAxisEngineMode *constant_chi(void)
{
	static const char* axes[] = {"omega", "phi", "tth"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_mode_operations);
}

static HklPseudoAxisEngineMode *constant_phi(void)
{
	static const char* axes[] = {"omega", "chi", "tth"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_mode_operations);
}

static HklPseudoAxisEngineMode *double_diffraction(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&double_diffraction_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, .value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, .value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, .value = 1,},
	};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions,
				      parameters),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_mode_operations);
}

static HklPseudoAxisEngineMode *psi_constant(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "tth"};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, .value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, .value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, .value = 1,},
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "psi"},
	};
	static const HklPseudoAxisEngineModeInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions,
				      parameters),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &psi_constant_vertical_mode_operations);
}

/***********************/
/* pseudo axes engines */
/***********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4c_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *default_mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	default_mode = bissector();
	hkl_pseudo_axis_engine_add_mode(self, default_mode);
	hkl_pseudo_axis_engine_select_mode(self, default_mode);

	hkl_pseudo_axis_engine_add_mode(self, constant_omega());
	hkl_pseudo_axis_engine_add_mode(self, constant_chi());
	hkl_pseudo_axis_engine_add_mode(self, constant_phi());
	hkl_pseudo_axis_engine_add_mode(self, double_diffraction());
	hkl_pseudo_axis_engine_add_mode(self, psi_constant());

	return self;
}
