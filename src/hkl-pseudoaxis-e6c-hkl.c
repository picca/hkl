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

#include <hkl/hkl-pseudoaxis-e6c.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_horizontal(const gsl_vector *x, void *params, gsl_vector *f)
{
	double mu, omega, gamma;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	omega = x_data[1];
	gamma = x_data[4];

	f_data[3] = omega;
	f_data[4] = gamma - 2 * fmod(mu, M_PI);

	return  GSL_SUCCESS;
}

static int bissector_vertical(const gsl_vector *x, void *params, gsl_vector *f)
{
	double omega, tth;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	omega = x_data[0];
	tth = x_data[3];

	f_data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector_horizontal(HklPseudoAxisEngine *engine,
								   HklGeometry *geometry,
								   HklDetector *detector,
								   HklSample *sample)
{
	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	return hkl_pseudo_axis_engine_solve_function(engine, bissector_horizontal);
}

static int hkl_pseudo_axis_engine_setter_func_bissector_vertical(HklPseudoAxisEngine *engine,
								 HklGeometry *geometry, HklDetector *detector,
								 HklSample *sample)
{
	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	return hkl_pseudo_axis_engine_solve_function(engine, bissector_vertical);
}

/***********************/
/* E6C PseudoAxeEngine */
/***********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e6c_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* bissector_vertical */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_bissector_vertical,
		0,
		4, "omega", "chi", "phi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_omega_vertical */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_omega_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "chi", "phi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_chi_vertical */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_chi_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "omega", "phi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi_vertical */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "omega", "chi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_phi */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_phi",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "phi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_omega */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_omega",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "omega", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* lifting_detector_mu */
	mode = hkl_pseudo_axis_engine_mode_new(
		"lifting_detector_mu",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_hkl_real,
		0,
		3, "mu", "gamma", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction vertical*/
	HklParameter h2;
	HklParameter k2;
	HklParameter l2;

	hkl_parameter_init(&h2, "h2", -1, 1, 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);
	hkl_parameter_init(&k2, "k2", -1, 1, 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);
	hkl_parameter_init(&l2, "l2", -1, 1, 1,
			   HKL_TRUE, HKL_TRUE,
			   NULL, NULL);

	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_vertical",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_double_diffraction_real,
		3, &h2, &k2, &l2,
		4, "omega", "chi", "phi", "delta");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* bissector_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_bissector_horizontal,
		0,
		5, "mu", "omega", "chi", "phi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction_horizontal */
	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction_horizontal",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_double_diffraction_real,
		3, &h2, &k2, &l2,
		4, "mu", "chi", "phi", "gamma");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
