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
 * Copyright (C) 2010-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>

#include <ccan/array_size/array_size.h>
#include <hkl/hkl-pseudoaxis-petra3.h>
#include <hkl/hkl-pseudoaxis-private.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/***********************/
/* numerical functions */
/***********************/

static int reflectivity(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double mu = x->data[0];
	const double gamma = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = mu - gamma;

	return  GSL_SUCCESS;
}

static int bissector_horizontal(const gsl_vector *x, void *params, gsl_vector *f)
{
	const double omega = x->data[0];
	const double delta = x->data[3];

	CHECK_NAN(x->data, x->size);

	RUBh_minus_Q(x->data, params, f->data);
	f->data[3] = delta - 2 * fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

/********/
/* mode */
/********/

static HklPseudoAxisEngineMode *zaxis_alpha_fixed()
{
	static const char *axes[] = {"omega", "delta", "gamma"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "zaxis + alpha-fixed",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, RUBh_minus_Q_func,
					       (size_t)0);
}

static HklPseudoAxisEngineMode *zaxis_beta_fixed()
{
	static const char *axes[] = {"mu", "delta", "gamma"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "zaxis + beta-fixed",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, RUBh_minus_Q_func,
					       (size_t)0);
}

static HklPseudoAxisEngineMode *zaxis_alpha_eq_beta()
{
	static const char *axes[] = {"mu", "omega", "delta", "gamma"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "zaxis + alpha=beta",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, reflectivity,
					       (size_t)0);
}

static HklPseudoAxisEngineMode *fourc_bissector_horizontal()
{
	static const char *axes[] = {"omega", "chi", "phi", "delta"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "4-circles bissecting horizontal",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, bissector_horizontal,
					       (size_t)0);
}

static HklPseudoAxisEngineMode *fourc_constant_omega_horizontal()
{
	static const char *axes[] = {"chi", "phi", "delta"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "4-circles constant omega horizontal",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, RUBh_minus_Q_func,
					       (size_t)0);
}

static HklPseudoAxisEngineMode *fourc_constant_chi_horizontal()
{
	static const char *axes[] = {"omega", "phi", "delta"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "4-circles constant chi horizontal",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, RUBh_minus_Q_func,
					       (size_t)0);
}

static HklPseudoAxisEngineMode *fourc_constant_phi_horizontal()
{
	static const char *axes[] = {"omega", "chi", "delta"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "4-circles constant phi horizontal",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &hkl_full_mode_operations,
					       1, RUBh_minus_Q_func,
					       (size_t)0);
}

/**********************/
/* pseudo axis engine */
/**********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_petra3_p09_eh2_hkl_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *default_mode;

	self = hkl_pseudo_axis_engine_hkl_new();

	default_mode = zaxis_alpha_fixed();
	hkl_pseudo_axis_engine_add_mode(self, default_mode);
	hkl_pseudo_axis_engine_select_mode(self, default_mode);

	hkl_pseudo_axis_engine_add_mode(self, zaxis_beta_fixed());
	hkl_pseudo_axis_engine_add_mode(self, zaxis_alpha_eq_beta());
	hkl_pseudo_axis_engine_add_mode(self, fourc_bissector_horizontal());
	hkl_pseudo_axis_engine_add_mode(self, fourc_constant_omega_horizontal());
	hkl_pseudo_axis_engine_add_mode(self, fourc_constant_chi_horizontal());
	hkl_pseudo_axis_engine_add_mode(self, fourc_constant_phi_horizontal());

	return self;
}
