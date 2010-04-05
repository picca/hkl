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

#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/*******/
/* hkl */
/*******/

static int bissector_func(const gsl_vector *x, void *params, gsl_vector *f)
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

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4cv_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	HklParameter h2;
	HklParameter k2;
	HklParameter l2;
	HklParameter psi;

	self = hkl_pseudo_axis_engine_hkl_new();

	/* hkl get/set bissector */
	mode = hkl_pseudo_axis_engine_mode_new(
		"bissector",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, bissector_func,
		(size_t)0,
		(size_t)4, "omega", "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_omega */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_omega",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_chi */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_chi",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "omega", "phi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* constant_phi */
	mode = hkl_pseudo_axis_engine_mode_new(
		"constant_phi",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, RUBh_minus_Q_func,
		(size_t)0,
		(size_t)3, "omega", "chi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* double_diffraction */
	hkl_parameter_init(&h2, "h2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&k2, "k2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&l2, "l2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);

	mode = hkl_pseudo_axis_engine_mode_new(
		"double_diffraction",
		NULL,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, double_diffraction_func,
		(size_t)3, h2, k2, l2,
		(size_t)4, "omega", "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	/* psi_constant */
	hkl_parameter_init(&h2, "h2", -1, 1, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&k2, "k2", -1, 0, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&l2, "l2", -1, 0, 1, HKL_TRUE, HKL_TRUE, NULL, NULL);
	hkl_parameter_init(&psi, "psi", -M_PI, 0, M_PI, HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	mode = hkl_pseudo_axis_engine_mode_new(
		"psi_constant",
		hkl_pseudo_axis_engine_mode_init_psi_constant_vertical_real,
		hkl_pseudo_axis_engine_mode_get_hkl_real,
		hkl_pseudo_axis_engine_mode_set_real,
		1, psi_constant_vertical_func,
		(size_t)4, h2, k2, l2, psi,
		(size_t)4, "omega", "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
