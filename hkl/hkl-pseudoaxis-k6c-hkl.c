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
 *          Maria-Teresa Nunez-Pardo-de-Verra <tnunez@mail.desy.de>
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf.h>

#include <hkl/hkl-pseudoaxis-k6c.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = fmod(omega, M_PI);
	f_data[4] = fmod(gamma - 2 * fmod(mu, M_PI), 2*M_PI);

	return  GSL_SUCCESS;
}

static int bissector_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = fmod(omega, M_PI);
	f_data[4] = fmod(gamma - 2 * fmod(mu, M_PI), 2*M_PI);


	return  GSL_SUCCESS;
}

static int constant_kphi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[1];
	kappa = x_data[2];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static int constant_kphi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[1];
	kappa = x_data[2];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static int constant_phi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, kphi;
	double omega, phi, p;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[1];
	kappa = x_data[2];
	kphi = x_data[3];

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));

	omega = komega + p - M_PI_2;
	phi = kphi + p + M_PI_2;

	f_data[3] = fmod(omega, M_PI);
	f_data[4] = phi;

	return  GSL_SUCCESS;
}

static int constant_phi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, kphi;
	double omega, phi, p;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[1];
	kappa = x_data[2];
	kphi = x_data[3];

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));

	omega = komega + p + M_PI_2;
	phi = kphi + p - M_PI_2;

	f_data[3] = fmod(omega, M_PI);
	f_data[4] = phi;

	return  GSL_SUCCESS;
}

static int bissector_v(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, delta, omega;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

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

static int constant_omega_v(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_chi_v(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_phi_v(const gsl_vector *x, void *params, gsl_vector *f)
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

static int double_diffraction_h(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	double_diffraction(x_data, params, f_data);

	komega = x_data[1];
	kappa = x_data[2];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[4] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

/************************/
/* K6CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k6c_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	HklParameter parameter;
	HklParameter h2;
	HklParameter k2;
	HklParameter l2;
	HklParameter psi;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* bissector_vertical */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, bissector_v,
		(size_t)0,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_omega_vertical */
	hkl_parameter_init(&parameter, "omega", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_omega_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, constant_omega_v,
		(size_t)1, parameter,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_chi_vertical */
	hkl_parameter_init(&parameter, "chi", -M_PI, 30. * HKL_DEGTORAD, M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_chi_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, constant_chi_v,
		(size_t)1, parameter,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi_vertical */
	hkl_parameter_init(&parameter, "phi", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, constant_phi_v,
		(size_t)1, parameter,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_kphi */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_kphi",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "kphi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_komega */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_komega",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "komega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_mu */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_mu",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "mu", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction vertical*/
	hkl_parameter_init(&h2, "h2", -1., 1., 1., HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&k2, "k2", -1., 1., 1., HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&l2, "l2", -1., 1., 1., HKL_TRUE, HKL_TRUE, NULL, NULL);

	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, double_diffraction_func,
		(size_t)3, h2, k2, l2,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* bissector_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		2, bissector_h_f1, bissector_h_f2,
		(size_t)0,
		(size_t)5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi_horizontal */
	hkl_parameter_init(&parameter, "phi", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		2, constant_phi_h_f1,constant_phi_h_f2,
		(size_t)1, parameter,
		(size_t)5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* horizontal kphi constant */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_kphi_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		2, constant_kphi_h_f1, constant_kphi_h_f2,
		(size_t)0,
		(size_t)4, "mu", "komega", "kappa", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, double_diffraction_h,
		(size_t)3, h2, k2, l2,
		(size_t)5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* psi_constant_vertical */
	hkl_parameter_init(&h2, "h2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&k2, "k2", -1, 0, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&l2, "l2", -1, 0, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&psi, "psi", -M_PI, 0, M_PI, HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"psi_constant_vertical",
		hkl_pseudo_axis_engine_mode_init_psi_constant_vertical_real,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, psi_constant_vertical_func,
		(size_t)4, h2, k2, l2, psi, 
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);	


	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
