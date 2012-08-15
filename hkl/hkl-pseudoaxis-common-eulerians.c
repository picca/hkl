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

static int kappa_to_eulerian(double komega, double kappa, double kphi,
			     double *omega, double *chi, double *phi,
			     double alpha, int solution)
{
	double Kappa = gsl_sf_angle_restrict_symm(kappa);
	double p = atan(tan(Kappa/2.) * cos(alpha));

	if (solution){
		*omega = komega + p - M_PI_2;
		*chi = 2 * asin(sin(Kappa/2.) * sin(alpha));
		*phi = kphi + p + M_PI_2;
	}else{
		*omega = komega + p + M_PI_2;
		*chi = -2 * asin(sin(Kappa/2.) * sin(alpha));
		*phi = kphi + p - M_PI_2;
	}

	return HKL_TRUE;
}

static int eulerian_to_kappa(double omega, double chi, double phi,
			     double *komega, double *kappa, double *kphi,
			     double alpha, double solution)
{
	int status = HKL_TRUE;

	if (fabs(chi) <= alpha * 2){
		double p = asin(tan(chi/2.)/tan(alpha));

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
	double komega, kappa, kphi;
	double *eulerians[3];
	HklPseudoAxis *pseudo_axis;
	uint i = 0;
	int solution;

	hkl_geometry_update(geometry);

	solution = (int)self->parameters[0].value;

	komega = ((HklParameter *)hkl_geometry_get_axis_by_name(geometry, "komega"))->value;
	kappa = ((HklParameter *)hkl_geometry_get_axis_by_name(geometry, "kappa"))->value;
	kphi = ((HklParameter *)hkl_geometry_get_axis_by_name(geometry, "kphi"))->value;

	list_for_each(&engine->pseudo_axes, pseudo_axis, list)
		eulerians[i++] = &pseudo_axis->parent.value;

	return kappa_to_eulerian(komega, kappa, kphi,
				 eulerians[0], eulerians[1], eulerians[2],
				 50 * HKL_DEGTORAD, solution);
}

static int hkl_pseudo_axis_engine_mode_set_eulerians_real(HklPseudoAxisEngineMode *self,
							  HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample,
							  HklError **error)
{
	int solution;
	int i = 0;
	HklPseudoAxis *pseudo_axis;
	double eulerians[3];
	double angles[3];

	solution = self->parameters[0].value;
	list_for_each(&engine->pseudo_axes, pseudo_axis, list)
		eulerians[i++] = pseudo_axis->parent.value;

	if(!eulerian_to_kappa(eulerians[0], eulerians[1], eulerians[2],
			      &angles[0], &angles[1], &angles[2],
			      50 * HKL_DEGTORAD, solution)){
		hkl_error_set(error, "unreachable solution : 0° < chi < 50°");
		return HKL_FALSE;
	}else
		hkl_pseudo_axis_engine_add_geometry(engine, angles);

	return HKL_TRUE;
}

static const HklPseudoAxisEngineModeOperations eulerians_mode_operations = {
	.get = hkl_pseudo_axis_engine_mode_get_eulerians_real,
	.set = hkl_pseudo_axis_engine_mode_set_eulerians_real,
};

HklPseudoAxisEngine *hkl_pseudo_axis_engine_eulerians_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	HklParameter parameter = { HKL_PARAMETER_DEFAULTS,
				   .name = "solution",
				   .range = {.min = 0, .max = 1},
				   .value = 1.
	};
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
	mode = hkl_pseudo_axis_engine_mode_new(
		"eulerians",
		&eulerians_mode_operations,
		0,
		(size_t)1, parameter,
		(size_t)3, "komega", "kappa", "kphi");
	hkl_pseudo_axis_engine_add_mode(self, mode);
	hkl_pseudo_axis_engine_select_mode(self, mode);

	return self;
}
