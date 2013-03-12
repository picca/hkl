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
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <hkl/ccan/array_size/array_size.h>

#include "hkl-parameter-private.h"
#include "hkl-pseudoaxis-auto-private.h"
#include "hkl-pseudoaxis-common-hkl-private.h"

/***********************/
/* numerical functions */
/***********************/

static int _bissector_horizontal_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double omega = x->data[1];
	const double gamma = x->data[4];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);
	f->data[4] = gamma - 2 * fmod(mu, M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_horizontal_func = {
	.function = _bissector_horizontal_func,
	.size = 5,
};

static int _bissector_vertical_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double omega = x->data[0];
	const double tth = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_vertical_func = {
	.function = _bissector_vertical_func,
	.size = 4,
};

/*********/
/* modes */
/*********/

static HklMode *bissector_vertical(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "delta"};
	static const HklFunction *functions[] = {&bissector_vertical_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_omega_vertical(void)
{
	static const char* axes[] = {"chi", "phi", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_chi_vertical(void)
{
	static const char* axes[] = {"omega", "phi", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_phi_vertical(void)
{
	static const char* axes[] = {"omega", "chi", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *lifting_detector_phi(void)
{
	static const char* axes[] = {"phi", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *lifting_detector_omega(void)
{
	static const char* axes[] = {"omega", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *lifting_detector_mu(void)
{
	static const char* axes[] = {"mu", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *double_diffraction_vertical(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "delta"};
	static const HklFunction *functions[] = {&double_diffraction_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, ._value = 1,},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *bissector_horizontal(void)
{
	static const char* axes[] = {"mu", "omega", "chi", "phi", "gamma"};
	static const HklFunction *functions[] = {&bissector_horizontal_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *double_diffraction_horizontal(void)
{
	static const char* axes[] = {"mu", "chi", "phi", "gamma"};
	static const HklFunction *functions[] = {&double_diffraction_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, ._value = 1,},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *psi_constant_vertical(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "delta"};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, ._value = 0,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, ._value = 0,},
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "psi"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &psi_constant_vertical_mode_operations);
}

static HklMode *psi_constant_horizontal(void)
{
	static const char* axes[] = {"omega", "chi", "phi", "gamma"};
	static const HklFunction *functions[] = {&psi_constant_vertical_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "h2", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "k2", .range = {.min=-1, .max=1}, ._value = 0,},
		{HKL_PARAMETER_DEFAULTS, .name = "l2", .range = {.min=-1, .max=1}, ._value = 0,},
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "psi"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &psi_constant_vertical_mode_operations);
}

static HklMode *constant_mu_horizontal(void)
{
	static const char* axes[] = {"chi", "phi", "gamma"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_full_mode_operations);
}

/***********************/
/* E6C PseudoAxeEngine */
/***********************/

HklEngine *hkl_engine_e6c_hkl_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new();

	default_mode = bissector_vertical();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_select_mode(self, default_mode);

	hkl_engine_add_mode(self, constant_omega_vertical());
	hkl_engine_add_mode(self, constant_chi_vertical());
	hkl_engine_add_mode(self, constant_phi_vertical());
	hkl_engine_add_mode(self, lifting_detector_phi());
	hkl_engine_add_mode(self, lifting_detector_omega());
	hkl_engine_add_mode(self, lifting_detector_mu());
	hkl_engine_add_mode(self, double_diffraction_vertical());
	hkl_engine_add_mode(self, bissector_horizontal());
	hkl_engine_add_mode(self, double_diffraction_horizontal());
	hkl_engine_add_mode(self, psi_constant_vertical());
	hkl_engine_add_mode(self, psi_constant_horizontal());
	hkl_engine_add_mode(self, constant_mu_horizontal());

	return self;
}
