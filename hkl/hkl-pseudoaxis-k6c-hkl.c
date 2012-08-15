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
#include <gsl/gsl_sf.h>

#include <hkl/hkl-pseudoaxis-k6c.h>
#include <hkl/hkl-pseudoaxis-private.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
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

static int bissector_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_kphi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_kphi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_phi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_phi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
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

static int bissector_v(const gsl_vector *x, void *params, gsl_vector *f)
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

static int constant_omega_v(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[0];
	const double kappa = x->data[1];
	double omega;
	HklPseudoAxisEngine *engine = params;
	double omega0 = engine->mode->parameters[0].value;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = omega0 - omega;

	return  GSL_SUCCESS;
}

static int constant_chi_v(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	double chi;
	HklPseudoAxisEngine *engine = params;
	double chi0 = engine->mode->parameters[0].value;

	CHECK_NAN(x->data, x->size);

	chi = 2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = chi0 - chi;

	return  GSL_SUCCESS;
}

static int constant_phi_v(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double kappa = x->data[1];
	const double kphi = x->data[2];
	double phi;
	HklPseudoAxisEngine *engine = params;
	double phi0 = engine->mode->parameters[0].value;

	CHECK_NAN(x->data, x->size);

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = phi0 - phi;

	return  GSL_SUCCESS;
}

static int double_diffraction_h(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double komega = x->data[1];
	const double kappa = x->data[2];
	double omega;

	CHECK_NAN(x->data, x->size);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	double_diffraction(x->data, params, f->data);
	f->data[4] = fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static int constant_incidence_func(const gsl_vector *x, void *params, gsl_vector *f)
{
	double incidence;
	double azimuth;
	HklPseudoAxisEngine *engine = params;
	HklVector n = {
		engine->mode->parameters[0].value,
		engine->mode->parameters[1].value,
		engine->mode->parameters[2].value,
	};
	double incidence0 = engine->mode->parameters[3].value;
	double azimuth0 = engine->mode->parameters[4].value;
	HklVector ki;
	HklVector Y;
	HklQuaternion q0;

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);

	/* compute the two angles */

	/* first check that the mode was already initialized if not
	 * the surface is oriented along the n_x, ny, nz axis for all
	 * diffractometer angles equal to zero */
	if(engine->mode->geometry_init)
		q0 = engine->mode->geometry_init->holders[0].q;
	else
		hkl_quaternion_init(&q0, 1, 0, 0, 0);

	hkl_quaternion_conjugate(&q0);
	hkl_vector_rotated_quaternion(&n, &q0);
	hkl_vector_rotated_quaternion(&n, &engine->geometry->holders[0].q);

	hkl_source_compute_ki(&engine->geometry->source, &ki);
	incidence = M_PI_2 - hkl_vector_angle(&n, &ki);

	hkl_vector_project_on_plan(&n, &ki);
	hkl_vector_init(&Y, 0, 1, 0);
	azimuth = hkl_vector_angle(&n, &Y);
	
	f->data[3] = incidence0 - incidence;
	f->data[4] = azimuth0 - azimuth;

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
	HklParameter x;
	HklParameter y;
	HklParameter z;
	HklParameter incidence;
	HklParameter azimuth;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* bissector_vertical [default] */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_vertical",
		&hkl_mode_operations,
		1, bissector_v,
		(size_t)0,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);
	hkl_pseudo_axis_engine_select_mode(self, mode);

	/* constant_omega_vertical */
	hkl_parameter_init(&parameter, "omega", -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_omega_vertical",
		&hkl_mode_operations,
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
		&hkl_mode_operations,
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
		&hkl_mode_operations,
		1, constant_phi_v,
		(size_t)1, parameter,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_kphi */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_kphi",
		&hkl_mode_operations,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "kphi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_komega */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_komega",
		&hkl_mode_operations,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "komega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_mu */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_mu",
		&hkl_mode_operations,
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
		&hkl_mode_operations,
		1, double_diffraction_func,
		(size_t)3, h2, k2, l2,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* bissector_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_horizontal",
		&hkl_mode_operations,
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
		&hkl_mode_operations,
		2, constant_phi_h_f1,constant_phi_h_f2,
		(size_t)1, parameter,
		(size_t)5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* horizontal kphi constant */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_kphi_horizontal",
		&hkl_mode_operations,
		2, constant_kphi_h_f1, constant_kphi_h_f2,
		(size_t)0,
		(size_t)4, "mu", "komega", "kappa", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_horizontal",
		&hkl_mode_operations,
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
		&psi_constant_vertical_mode_operations,
		1, psi_constant_vertical_func,
		(size_t)4, h2, k2, l2, psi,
		(size_t)4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_incidence */
	hkl_parameter_init(&x, "x", -1, 0, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&y, "y", -1, 0, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&z, "z", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&incidence, "incidence", -M_PI, 0, M_PI, HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	hkl_parameter_init(&azimuth, "azimuth", -M_PI, M_PI_2, M_PI, HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_incidence",
		&hkl_mode_operations,
		1, constant_incidence_func,
		(size_t)5, x, y, z, incidence, azimuth,
		(size_t)5, "komega", "kappa", "kphi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	return self;
}
