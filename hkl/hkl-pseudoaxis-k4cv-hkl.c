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
#include <gsl/gsl_errno.h>              // for ::GSL_SUCCESS
#include <gsl/gsl_sys.h>                // for gsl_isnan
#include <gsl/gsl_vector_double.h>      // for gsl_vector
#include <math.h>                       // for atan, cos, tan, fmod, sin, etc
#include <sys/types.h>                  // for uint
#include "hkl-parameter-private.h"
#include "hkl-pseudoaxis-auto-private.h"  // for HklFunction, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for RUBh_minus_Q, etc
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_add_mode, etc
#include "hkl/ccan/array_size/array_size.h"  // for ARRAY_SIZE
#include "hkl.h"                        // for HklEngine, HklMode, etc

/***********************/
/* numerical functions */
/***********************/

static int _bissector_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	const double tth = x->data[3];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(tth - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_f1 = {
	.function = _bissector_f1,
	.size = 4,
};

static int _bissector_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	const double tth = x->data[3];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(tth - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_f2 = {
	.function = _bissector_f2,
	.size = 4,
};

static int _constant_omega_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double const komega = x->data[0];
	double const kappa = x->data[1];
	double omega;
	HklEngine *engine = params;
	double omega0;
	uint shit;

	hkl_parameter_list_values_get(&engine->mode->parameters, &omega0, &shit);

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = omega0 - omega;

	return  GSL_SUCCESS;
}

static const HklFunction constant_omega_f1 = {
	.function = _constant_omega_f1,
	.size = 4,
};

static int _constant_omega_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	double omega;
	HklEngine *engine = params;
	double omega0;
	uint shit;

	hkl_parameter_list_values_get(&engine->mode->parameters, &omega0, &shit);

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = omega0 - omega;

	return  GSL_SUCCESS;
}

static const HklFunction constant_omega_f2 = {
	.function = _constant_omega_f2,
	.size = 4,
};

static int _constant_chi_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	double chi;
	HklEngine *engine = params;
	double chi0;
	uint shit;

	hkl_parameter_list_values_get(&engine->mode->parameters, &chi0, &shit);

	CHECK_NAN(x->data, x->size);

	chi = 2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = chi0 - chi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_chi_f1 = {
	.function = _constant_chi_f1,
	.size = 4,
};

static int _constant_chi_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	double chi;
	HklEngine *engine = params;
	double chi0;
	uint shit;

	hkl_parameter_list_values_get(&engine->mode->parameters, &chi0, &shit);

	CHECK_NAN(x->data, x->size);

	chi = -2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = chi0 - chi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_chi_f2 = {
	.function = _constant_chi_f2,
	.size = 4,
};

static int _constant_phi_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	const double kphi = x->data[2];
	double phi;
	HklEngine *engine = params;
	double phi0;
	uint shit;

	hkl_parameter_list_values_get(&engine->mode->parameters, &phi0, &shit);

	CHECK_NAN(x->data, x->size);

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = phi0 - phi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_phi_f1 = {
	.function = _constant_phi_f1,
	.size = 4,
};

static int _constant_phi_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	const double kphi = x->data[2];
	double phi;
	HklEngine *engine = params;
	double phi0;
	uint shit;

	hkl_parameter_list_values_get(&engine->mode->parameters, &phi0, &shit);

	CHECK_NAN(x->data, x->size);

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = phi0 - phi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_phi_f2 = {
	.function = _constant_phi_f2,
	.size = 4,
};

/********/
/* mode */
/********/

static HklMode *bissector(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&bissector_f1, &bissector_f2};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_omega(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&constant_omega_f1, &constant_omega_f2};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "omega"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_chi(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&constant_chi_f1, &constant_chi_f2};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "chi"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_phi(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
	static const HklFunction *functions[] = {&constant_phi_f1, &constant_phi_f2};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "phi"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *double_diffraction(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
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

static HklMode *psi_constant(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "tth"};
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

/**********************/
/* pseudo axis engine */
/**********************/

HklEngine *hkl_engine_k4cv_hkl_new(void)
{
	HklEngine *self;
	HklMode *default_mode;

	self = hkl_engine_hkl_new();

	default_mode = bissector();
	hkl_engine_add_mode(self, default_mode);
	hkl_engine_select_mode(self, default_mode);

	hkl_engine_add_mode(self, constant_omega());
	hkl_engine_add_mode(self, constant_chi());
	hkl_engine_add_mode(self, constant_phi());
	hkl_engine_add_mode(self, double_diffraction());
	hkl_engine_add_mode(self, psi_constant());

	return self;
}
