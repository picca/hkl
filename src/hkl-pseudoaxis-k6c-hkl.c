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
#include <gsl/gsl_sf.h>

#include <hkl/hkl-pseudoaxis-k6c.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/***********************/
/* numerical functions */
/***********************/

static void multiply(HklGeometryList *list, HklGeometry *geometry)
{
	HklGeometry *copy;
	double komega, komegap;
	double kappa, kappap;
	double kphi, kphip;
	double omega;
	double phi;
	double p;

	komega = geometry->axes[1].parent.value;
	kappa = geometry->axes[2].parent.value;
	kphi = geometry->axes[3].parent.value;

	p = atan(tan(kappa/2.) * cos(50 * HKL_DEGTORAD));
	omega = komega + p - M_PI_2;
	phi = kphi + p + M_PI_2;

	komegap = gsl_sf_angle_restrict_symm(2*omega - komega);
	kappap = -kappa;
	kphip = gsl_sf_angle_restrict_symm(2*phi - kphi);

	copy = hkl_geometry_new_copy(geometry);
	hkl_axis_set_value(&copy->axes[1], komegap);
	hkl_axis_set_value(&copy->axes[2], kappap);
	hkl_axis_set_value(&copy->axes[3], kphip);

	hkl_geometry_update(copy);
	hkl_geometry_list_add(list, copy);
}

static int bissector_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = omega;
	f_data[4] = fmod(gamma - 2 * fmod(mu, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static int bissector_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = omega;
	f_data[4] = fmod(gamma - 2 * fmod(mu, M_PI), 2*M_PI);


	return  GSL_SUCCESS;
}

static int constant_kphi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = omega;

	return  GSL_SUCCESS;
}

static int constant_kphi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = omega;

	return  GSL_SUCCESS;
}

static int constant_phi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, kphi;
	double omega, phi, p;
	size_t i;
	HklPseudoAxisEngine *engine;
//	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	kphi = x_data[3];
	gamma = x_data[4];

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));

	omega = komega + p - M_PI_2;
	phi = kphi + p + M_PI_2;

	f_data[3] = omega;
	f_data[4] = phi;

	return  GSL_SUCCESS;
}

static int constant_phi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, kphi;
	double omega, phi, p;
	size_t i;
	HklPseudoAxisEngine *engine;
//	double p0 = engine->mode->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	kphi = x_data[3];
	gamma = x_data[4];

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));

	omega = komega + p + M_PI_2;
	phi = kphi + p - M_PI_2;

	f_data[3] = omega;
	f_data[4] = phi;

	return  GSL_SUCCESS;
}

static int bissector_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, delta, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	delta = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = fmod(delta - 2 * fmod(omega, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

/*
static int bissector_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, delta, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	delta = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = fmod(delta - 2 * fmod(omega, M_PI), 2*M_PI);


	return  GSL_SUCCESS;
}
*/

static int constant_omega_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_omega_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_chi_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_chi_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_phi_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_phi_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
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

static int double_diffraction_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	double_diffraction(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[4] = omega;

	return  GSL_SUCCESS;
}

static int double_diffraction_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	double_diffraction(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[4] = omega;


	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector_h(HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_phi_h(HklPseudoAxisEngine *engine,
							      HklGeometry *geometry,
							      HklDetector *detector,
							      HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_kphi_h(HklPseudoAxisEngine *engine,
							      HklGeometry *geometry,
							      HklDetector *detector,
							      HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_kphi_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_kphi_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_bissector_v(HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_v_f1);
	//res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_v_f2);
	hkl_geometry_list_multiply_function(engine->engines->geometries, multiply);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_omega_v(HklPseudoAxisEngine *engine,
							       HklGeometry *geometry,
							       HklDetector *detector,
							       HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_omega_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_omega_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_chi_v(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector *detector,
							     HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_chi_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_chi_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_phi_v(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector *detector,
							     HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_mode_set_double_diffraction_horizontal_real(HklPseudoAxisEngine *engine,
										 HklGeometry *geometry,
										 HklDetector *detector,
										 HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, double_diffraction_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, double_diffraction_h_f2);

	return res;
}

/************************/
/* K6CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k6c_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	HklParameter parameter;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* bissector_vertical */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_bissector_v,
		0,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_omega_vertical */
	hkl_parameter_init(&parameter, "omega", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_omega_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_omega_v,
		1, &parameter,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_chi_vertical */
	hkl_parameter_init(&parameter, "chi", -M_PI, 30. * HKL_DEGTORAD, M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_chi_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_chi_v,
		1, &parameter,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi_vertical */
	hkl_parameter_init(&parameter, "phi", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_phi_v,
		1, &parameter,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "kphi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction vertical*/
	HklParameter h2;
	HklParameter k2;
	HklParameter l2;

	hkl_parameter_init(&h2, "h2", -1., 1., 1.,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);

	hkl_parameter_init(&k2, "k2", -1., 1., 1.,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);

	hkl_parameter_init(&l2, "l2", -1., 1., 1.,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);

	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_double_diffraction_real,
		3, &h2, &k2, &l2,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* bissector_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_bissector_h,
		0,
		5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi_horizontal */
	hkl_parameter_init(&parameter, "phi", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_phi_h,
		1, &parameter,
		5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* horizontal kphi constant */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_kphi_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_constant_kphi_h,
		0,
		4, "mu", "komega", "kappa", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_double_diffraction_horizontal_real,
		3, &h2, &k2, &l2,
		5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
