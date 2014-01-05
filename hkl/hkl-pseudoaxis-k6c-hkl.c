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
#include <math.h>                       // for fmod, atan, cos, tan, etc
#include <sys/types.h>                  // for uint
#include "hkl-geometry-private.h"       // for _HklGeometry, HklHolder
#include "hkl-parameter-private.h"      // for _HklParameter, etc
#include "hkl-pseudoaxis-auto-private.h"  // for HklFunction, etc
#include "hkl-pseudoaxis-common-hkl-private.h"  // for RUBh_minus_Q, etc
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_add_mode, etc
#include "hkl-quaternion-private.h"     // for hkl_quaternion_conjugate, etc
#include "hkl-source-private.h"         // for hkl_source_compute_ki
#include "hkl-vector-private.h"         // for HklVector, hkl_vector_angle, etc
#include "hkl.h"                        // for HklMode, HklParameter, etc
#include "hkl/ccan/array_size/array_size.h"  // for ARRAY_SIZE
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_item

/***********************/
/* numerical functions */
/***********************/

static int _bissector_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double komega = x->data[1];
	const double kappa = x->data[2];
	const double gamma = x->data[4];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);
	f->data[4] = fmod(gamma - 2 * fmod(mu, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_h_f1 = {
	.function = _bissector_h_f1,
	.size = 5,
};

static int _bissector_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double komega = x->data[1];
	const double kappa = x->data[2];
	const double gamma = x->data[4];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);
	f->data[4] = fmod(gamma - 2 * fmod(mu, M_PI), 2*M_PI);


	return  GSL_SUCCESS;
}

static const HklFunction bissector_h_f2 = {
	.function = _bissector_h_f2,
	.size = 5,
};

static int _constant_kphi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[1];
	const double kappa = x->data[2];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction constant_kphi_h_f1 = {
	.function = _constant_kphi_h_f1,
	.size = 4,
};

static int _constant_kphi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[1];
	const double kappa = x->data[2];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction constant_kphi_h_f2 = {
	.function = _constant_kphi_h_f2,
	.size = 4,
};

static int _constant_phi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[1];
	const double kappa = x->data[2];
	const double kphi = x->data[3];
	double omega, phi, p;

	CHECK_NAN(x->data, x->size);

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));
	omega = komega + p - M_PI_2;
	phi = kphi + p + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);
	f->data[4] = phi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_phi_h_f1 = {
	.function = _constant_phi_h_f1,
	.size = 5,
};

static int _constant_phi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[1];
	const double kappa = x->data[2];
	const double kphi = x->data[3];
	double omega, phi, p;

	CHECK_NAN(x->data, x->size);

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));
	omega = komega + p + M_PI_2;
	phi = kphi + p - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(omega, M_PI);
	f->data[4] = phi;

	return  GSL_SUCCESS;
}

static const HklFunction constant_phi_h_f2 = {
	.function = _constant_phi_h_f2,
	.size = 5,
};

static int _bissector_v(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	const double delta = x->data[3];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = fmod(delta - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction bissector_v = {
	.function = _bissector_v,
	.size = 4,
};

static int _constant_omega_v(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
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

static const HklFunction constant_omega_v = {
	.function = _constant_omega_v,
	.size = 4,
};

static int _constant_chi_v(const gsl_vector *x, void *params, gsl_vector *f)
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

static const HklFunction constant_chi_v = {
	.function = _constant_chi_v,
	.size = 4,
};

static int _constant_phi_v(const gsl_vector *x, void *params, gsl_vector *f)
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

static const HklFunction constant_phi_v = {
	.function = _constant_phi_v,
	.size = 4,
};

static int _double_diffraction_h(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[1];
	const double kappa = x->data[2];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	_double_diffraction(x->data, params, f->data);
	f->data[4] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static const HklFunction double_diffraction_h = {
	.function = _double_diffraction_h,
	.size = 5,
};

static int _constant_incidence_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	static const HklVector Y = {
		.data = {0, 1, 0},
	};
	double incidence;
	double azimuth;
	HklEngine *engine = params;
	HklModeAutoWithInit *mode = container_of(engine->mode, HklModeAutoWithInit, mode);
	double parameters[5];
	uint shit;
	HklVector n;
	double incidence0;
	double azimuth0;
	HklVector ki;

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);

	/* get the mode parameters */
	hkl_parameter_list_values_get(&engine->mode->parameters,
				      parameters, &shit);
	n.data[0] = parameters[0];
	n.data[1] = parameters[1];
	n.data[2] = parameters[2];
	incidence0 = parameters[3];
	azimuth0 = parameters[4];

	/* compute the two angles */


	/* first check that the mode was already initialized if not
	 * the surface is oriented along the nx, ny, nz axis for all
	 * diffractometer angles equal to zero */
	if(mode->geometry){
		HklQuaternion q0 = darray_item(mode->geometry->holders, 0)->q;

		hkl_quaternion_conjugate(&q0);
		hkl_vector_rotated_quaternion(&n, &q0);
	}

	hkl_vector_rotated_quaternion(&n, &darray_item(engine->geometry->holders, 0)->q);

	hkl_source_compute_ki(&engine->geometry->source, &ki);
	incidence = M_PI_2 - hkl_vector_angle(&n, &ki);

	hkl_vector_project_on_plan(&n, &ki);
	azimuth = hkl_vector_angle(&n, &Y);

	f->data[3] = incidence0 - incidence;
	f->data[4] = azimuth0 - azimuth;

	return  GSL_SUCCESS;
}

static const HklFunction constant_incidence_func = {
	.function = _constant_incidence_func,
	.size = 5,
};

/********/
/* mode */
/********/

static HklMode *bissector_vertical(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "delta"};
	static const HklFunction *functions[] = {&bissector_v};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_omega_vertical(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "delta"};
	static const HklFunction *functions[] = {&constant_omega_v};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "omega"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_chi_vertical(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "delta"};
	static const HklFunction *functions[] = {&constant_chi_v};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "chi"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_phi_vertical(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "delta"};
	static const HklFunction *functions[] = {&constant_phi_v};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "phi"},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *lifting_detector_kphi(void)
{
	static const char* axes[] = {"kphi", "gamma", "delta"};
	static const HklFunction *functions[] = {&RUBh_minus_Q_func};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *lifting_detector_komega(void)
{
	static const char* axes[] = {"komega", "gamma", "delta"};
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
	static const char* axes[] = {"komega", "kappa", "kphi", "delta"};
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
	static const char* axes[] = {"mu", "komega", "kappa", "kphi", "gamma"};
	static const HklFunction *functions[] = {&bissector_h_f1, &bissector_h_f2};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_phi_horizontal(void)
{
	static const char* axes[] = {"mu", "komega", "kappa", "kphi", "gamma"};
	static const HklFunction *functions[] = {&constant_phi_h_f1, &constant_phi_h_f2};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "phi",},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *constant_kphi_horizontal(void)
{
	static const char* axes[] = {"mu", "komega", "kappa", "gamma"};
	static const HklFunction *functions[] = {&constant_kphi_h_f1, &constant_kphi_h_f2};
	static const HklModeAutoInfo info = {
		INFO_AUTO(__func__, axes, functions),
	};

	return hkl_mode_auto_new(&info,
				 &hkl_mode_operations);
}

static HklMode *double_diffraction_horizontal(void)
{
	static const char* axes[] = {"mu", "komega", "kappa", "kphi", "gamma"};
	static const HklFunction *functions[] = {&double_diffraction_h};
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
	static const char* axes[] = {"komega", "kappa", "kphi", "delta"};
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

static HklMode *constant_incidence(void)
{
	static const char* axes[] = {"komega", "kappa", "kphi", "gamma", "delta"};
	static const HklFunction *functions[] = {&constant_incidence_func};
	static const HklParameter parameters[] = {
		{HKL_PARAMETER_DEFAULTS, .name = "x", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "y", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS, .name = "z", .range = {.min=-1, .max=1}, ._value = 1,},
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "incidence"},
		{HKL_PARAMETER_DEFAULTS_ANGLE, .name = "aximuth", ._value = M_PI_2,},
	};
	static const HklModeAutoInfo info = {
		INFO_AUTO_WITH_PARAMS(__func__, axes, functions, parameters),
	};

	return hkl_mode_auto_with_init_new(&info,
					   &constant_incidence_mode_operations);
}

/**********************/
/* pseudo axis engine */
/**********************/

HklEngine *hkl_engine_k6c_hkl_new(void)
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
	hkl_engine_add_mode(self, lifting_detector_kphi());
	hkl_engine_add_mode(self, lifting_detector_komega());
	hkl_engine_add_mode(self, lifting_detector_mu());
	hkl_engine_add_mode(self, double_diffraction_vertical());
	hkl_engine_add_mode(self, bissector_horizontal());
	hkl_engine_add_mode(self, constant_phi_horizontal());
	hkl_engine_add_mode(self, constant_kphi_horizontal());
	hkl_engine_add_mode(self, double_diffraction_horizontal());
	hkl_engine_add_mode(self, psi_constant_vertical());
	hkl_engine_add_mode(self, constant_incidence());

	return self;
}
