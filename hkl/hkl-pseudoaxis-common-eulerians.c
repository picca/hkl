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

	return HKL_SUCCESS;
}

static int eulerian_to_kappa(double omega, double chi, double phi,
			     double *komega, double *kappa, double *kphi,
			     double alpha, double solution)
{
	int status = HKL_SUCCESS;

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
		status = HKL_FAIL;

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
	int solution;

	hkl_geometry_update(geometry);

	solution = self->parameters[0].value;

	komega = ((HklParameter *)hkl_geometry_get_axis_by_name(geometry, "komega"))->value;
	kappa = ((HklParameter *)hkl_geometry_get_axis_by_name(geometry, "kappa"))->value;
	kphi = ((HklParameter *)hkl_geometry_get_axis_by_name(geometry, "kphi"))->value;

	return kappa_to_eulerian(komega, kappa, kphi,
				 &((HklParameter *)engine->pseudoAxes[0])->value,
				 &((HklParameter *)engine->pseudoAxes[1])->value,
				 &((HklParameter *)engine->pseudoAxes[2])->value,
				 50 * HKL_DEGTORAD, solution);
}

static int hkl_pseudo_axis_engine_mode_set_eulerians_real(HklPseudoAxisEngineMode *self,
							  HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample,
							  HklError **error)
{
	int status = HKL_SUCCESS;
	int solution;

	double angles[3];

	solution = self->parameters[0].value;

	status |= eulerian_to_kappa(((HklParameter *)engine->pseudoAxes[0])->value,
				    ((HklParameter *)engine->pseudoAxes[1])->value,
				    ((HklParameter *)engine->pseudoAxes[2])->value,
				    &angles[0], &angles[1], &angles[2],
				    50 * HKL_DEGTORAD, solution);

	if (status == HKL_SUCCESS)
		hkl_pseudo_axis_engine_add_geometry(engine, angles);

	return status;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_eulerians_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineMode *mode;
	HklParameter parameter = {"solution", {0, 1}, 1., 0};

	self = hkl_pseudo_axis_engine_new("eulerians", 3, "omega", "chi", "phi");

	/* omega */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[0],
			   "omega",
			   -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	/* chi */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[1],
			   "chi",
			   -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);
	/* phi */
	hkl_parameter_init((HklParameter *)self->pseudoAxes[2],
			   "phi",
			   -M_PI, 0., M_PI,
			   HKL_TRUE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	/* eulerians */
	mode = hkl_pseudo_axis_engine_mode_new(
		"eulerians",
		NULL,
		hkl_pseudo_axis_engine_mode_get_eulerians_real,
		hkl_pseudo_axis_engine_mode_set_eulerians_real,
		0,
		(size_t)1, parameter,
		(size_t)3, "komega", "kappa", "kphi");
	hkl_pseudo_axis_engine_add_mode(self, mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
