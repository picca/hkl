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
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_sf_trig.h>
#include <ccan/array_size/array_size.h>
#include <hkl/hkl-pseudoaxis-auto.h>
#include <hkl/hkl-pseudoaxis-common-eulerians.h>

static int kappa_to_eulerian(const double angles[], double eulerians[],
			     double alpha, int solution)
{
	const double komega = angles[0];
	const double kappa = gsl_sf_angle_restrict_symm(angles[1]);
	const double kphi = angles[2];
	const double p = atan(tan(kappa/2.) * cos(alpha));
	double *omega = &eulerians[0];
	double *chi = &eulerians[1];
	double *phi = &eulerians[2];

	if (solution){
		*omega = komega + p - M_PI_2;
		*chi = 2 * asin(sin(kappa/2.) * sin(alpha));
		*phi = kphi + p + M_PI_2;
	}else{
		*omega = komega + p + M_PI_2;
		*chi = -2 * asin(sin(kappa/2.) * sin(alpha));
		*phi = kphi + p - M_PI_2;
	}

	return HKL_TRUE;
}

static int eulerian_to_kappa(const double eulerians[], double angles[],
			     double alpha, double solution)
{
	int status = HKL_TRUE;
	const double omega = eulerians[0];
	const double chi = eulerians[1];
	const double phi = eulerians[2];
	double *komega = &angles[0];
	double *kappa = &angles[1];
	double *kphi = &angles[2];

	if (fabs(chi) <= alpha * 2){
		const double p = asin(tan(chi/2.)/tan(alpha));

		if (solution){
			*komega = omega - p + M_PI_2;
			*kappa = 2 * asin(sin(chi/2.)/sin(alpha));
			*kphi = phi - p - M_PI_2;
		}else{
			*komega = omega + p - M_PI_2;
			*kappa = -2 * asin(sin(chi/2.)/sin(alpha));
			*kphi = phi + p + M_PI_2;
		}
	}else
		status = HKL_FALSE;

	return status;
}

static int hkl_pseudo_axis_engine_mode_get_eulerians_real(HklPseudoAxisEngineMode *self,
							  HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample,
							  HklError **error)
{
	const double angles[] = {
		hkl_geometry_get_axis_by_name(geometry, "komega")->parent_instance.value,
		hkl_geometry_get_axis_by_name(geometry, "kappa")->parent_instance.value,
		hkl_geometry_get_axis_by_name(geometry, "kphi")->parent_instance.value,
	};
	double values[3];
	int solution;

	hkl_geometry_update(geometry);

	solution = (int)self->parameters[0].value;
	kappa_to_eulerian(angles, values, 50 * HKL_DEGTORAD, solution);
	hkl_pseudo_axis_engine_set_values(engine, values, 3);

	return HKL_TRUE;
}

static int hkl_pseudo_axis_engine_mode_set_eulerians_real(HklPseudoAxisEngineMode *self,
							  HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample,
							  HklError **error)
{
	int solution;
	uint n_values = engine->info->n_pseudo_axes;
	double values[n_values];
	double angles[3];

	solution = self->parameters[0].value;
	hkl_pseudo_axis_engine_get_values(engine, values, &n_values);

	if(!eulerian_to_kappa(values, angles, 50 * HKL_DEGTORAD, solution)){
		hkl_error_set(error, "unreachable solution : 0° < chi < 50°");
		return HKL_FALSE;
	}else
		hkl_pseudo_axis_engine_add_geometry(engine, angles);

	return HKL_TRUE;
}


static HklPseudoAxisEngineMode *mode_eulerians()
{
	HklPseudoAxisEngineMode *mode;
	static const char *axes[] = {"komega", "kappa", "kphi"};
	static const HklPseudoAxisEngineModeInfo info = {
		.name = "eulerians",
		.axes = axes,
		.n_axes = ARRAY_SIZE(axes),
	};
	static const HklParameter parameter = {
		HKL_PARAMETER_DEFAULTS,
		.name = "solution",
		.range = {.min = 0, .max = 1},
		.value = 1.
	};
	static const HklPseudoAxisEngineModeOperations operations = {
		HKL_MODE_OPERATIONS_DEFAULTS,
		.get = hkl_pseudo_axis_engine_mode_get_eulerians_real,
		.set = hkl_pseudo_axis_engine_mode_set_eulerians_real,
	};

	return hkl_pseudo_axis_engine_mode_new(&info,
					       &operations,
					       0,
					       (size_t)1, parameter);
};

HklPseudoAxisEngine *hkl_pseudo_axis_engine_eulerians_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	static const HklPseudoAxis omega = {
		.parent = { HKL_PARAMETER_DEFAULTS_ANGLE, .name = "omega"}
	};
	static const HklPseudoAxis chi = {
		.parent = { HKL_PARAMETER_DEFAULTS_ANGLE, .name = "chi"}
	};
	static const HklPseudoAxis phi = {
		.parent = {HKL_PARAMETER_DEFAULTS_ANGLE, .name = "phi"}
	};
	static const HklPseudoAxis *pseudo_axes[] = {&omega, &chi, &phi};
	static HklPseudoAxisEngineInfo info = {
		.name = "eulerians",
		.pseudo_axes = pseudo_axes,
		.n_pseudo_axes = ARRAY_SIZE(pseudo_axes),
	};

	self = hkl_pseudo_axis_engine_new(&info);

	/* eulerians [default] */
	mode = mode_eulerians();
	hkl_pseudo_axis_engine_add_mode(self, mode);
	hkl_pseudo_axis_engine_select_mode(self, mode);

	return self;
}
