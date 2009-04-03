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
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <hkl/hkl-pseudoaxis-k4cv.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, tth, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	tth = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = fmod(tth - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static int bissector_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, tth, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	tth = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = fmod(tth - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static int constant_omega_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = p0 - omega;

	return  GSL_SUCCESS;
}

static int constant_omega_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = p0 - omega;

	return  GSL_SUCCESS;
}

static int constant_chi_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, chi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];

	chi = 2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	f_data[3] = p0 - chi;

	return  GSL_SUCCESS;
}

static int constant_chi_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, chi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];

	chi = -2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	f_data[3] = p0 - chi;

	return  GSL_SUCCESS;
}

static int constant_phi_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, kphi, phi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];
	kphi = x_data[2];

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = p0 - phi;

	return  GSL_SUCCESS;
}

static int constant_phi_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, kphi, phi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];
	kphi = x_data[2];

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = p0 - phi;

	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

/* HKL bissector */
static int hkl_pseudo_axis_engine_setter_func_bissector(HklPseudoAxisEngine *engine,
							HklGeometry *geometry,
							HklDetector *detector,
							HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_f2);

	return res;
}

/* HKL constant omega */
static int hkl_pseudo_axis_engine_setter_func_constant_omega(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector *detector,
							     HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_omega_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_omega_f2);

	return res;
}

/* HKL constant chi */
static int hkl_pseudo_axis_engine_setter_func_constant_chi(HklPseudoAxisEngine *engine,
							   HklGeometry *geometry,
							   HklDetector *detector,
							   HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_chi_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_chi_f2);

	return res;
}

/* HKL constant phi */
static int hkl_pseudo_axis_engine_setter_func_constant_phi(HklPseudoAxisEngine *engine,
							   HklGeometry *geometry,
							   HklDetector *detector,
							   HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_f2);

	return res;
}

/************************/
/* K4CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k4cv_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	HklParameter parameter;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* hkl get/set bissector */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_bissector,
		0,
		4, "komega", "kappa", "kphi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_omega */
	hkl_parameter_init(&parameter, "omega", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_omega",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_omega,
		1, &parameter,
		4, "komega", "kappa", "kphi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_chi */
	hkl_parameter_init(&parameter, "chi", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_chi",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_chi,
		1, &parameter,
		4, "komega", "kappa", "kphi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi */
	hkl_parameter_init(&parameter, "phi", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_phi,
		1, &parameter,
		4, "komega", "kappa", "kphi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction */
	HklParameter h2;
	HklParameter k2;
	HklParameter l2;

	hkl_parameter_init(&h2, "h2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&k2, "k2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&l2, "l2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);

	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_double_diffraction_real,
		3, &h2, &k2, &l2,
		4, "komega", "kappa", "kphi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
