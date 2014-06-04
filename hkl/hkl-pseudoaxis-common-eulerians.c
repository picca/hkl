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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Jens Krüger <Jens.Krueger@frm2.tum.de>
 */
#include <gsl/gsl_sf_trig.h>            // for gsl_sf_angle_restrict_symm
#include <math.h>                       // for sin, asin, M_PI_2, tan, etc
#include <stdlib.h>                     // for free
#include <sys/types.h>                  // for uint
#include "hkl-error-private.h"          // for hkl_error_set
#include "hkl-geometry-private.h"
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for _HklParameter, etc
#include "hkl-pseudoaxis-common-eulerians-private.h"
#include "hkl-pseudoaxis-private.h"     // for _HklPseudoAxis, etc
#include "hkl.h"                        // for HklParameter, HklPseudoAxis, etc
#include "hkl/ccan/array_size/array_size.h"  // for ARRAY_SIZE
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_item

static int kappa_to_eulerian(const double angles[],
			     double *omega, double *chi, double *phi,
			     double alpha, int solution)
{
	const double komega = angles[0];
	const double kappa = gsl_sf_angle_restrict_symm(angles[1]);
	const double kphi = angles[2];
	const double p = atan(tan(kappa/2.) * cos(alpha));

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

static int eulerian_to_kappa(const double omega, const double chi, const double phi,
			     double angles[],
			     double alpha, double solution)
{
	int status = HKL_TRUE;
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

/***************************/
/* HklMode */
/***************************/

static int hkl_mode_get_eulerians_real(HklMode *self,
				       HklEngine *engine,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample,
				       HklError **error)
{
	HklEngineEulerians *eulerians;
	const double angles[] = {
		hkl_parameter_value_get(
			hkl_geometry_get_axis_by_name(geometry, "komega")),
		hkl_parameter_value_get(
			hkl_geometry_get_axis_by_name(geometry, "kappa")),
		hkl_parameter_value_get(
			hkl_geometry_get_axis_by_name(geometry, "kphi")),
	};
	double values[3];
	double solution;
	HklParameter *parameter;

	hkl_geometry_update(geometry);

	solution = darray_item(self->parameters, 0)->_value;

	eulerians = container_of(engine, HklEngineEulerians, engine);
	kappa_to_eulerian(angles,
			  &eulerians->omega->_value,
			  &eulerians->chi->_value,
			  &eulerians->phi->_value,
			  50 * HKL_DEGTORAD, solution);

	return HKL_TRUE;
}

static int hkl_mode_set_eulerians_real(HklMode *self,
				       HklEngine *engine,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample,
				       HklError **error)
{
	double solution;
	uint n_values = engine->info->n_pseudo_axes;
	HklEngineEulerians *engine_eulerians;
	double angles[3];

	solution = darray_item(self->parameters, 0)->_value;
	engine_eulerians = container_of(engine, HklEngineEulerians, engine);
	if(!eulerian_to_kappa(engine_eulerians->omega->_value,
			      engine_eulerians->chi->_value,
			      engine_eulerians->phi->_value,
			      angles, 50 * HKL_DEGTORAD, solution)){
		hkl_error_set(error, "unreachable solution : 0° < chi < 50°");
		return HKL_FALSE;
	}else
		hkl_engine_add_geometry(engine, angles);

	return HKL_TRUE;
}


static HklMode *mode_eulerians()
{
	HklMode *mode;
	static const char *axes[] = {"komega", "kappa", "kphi"};
	static const HklParameter parameters[] = {
		{ HKL_PARAMETER_DEFAULTS, .name = "solution", .range = {.max = 1}, ._value = 1,},
	};
	static const HklModeInfo info = {
		INFO_WITH_PARAMS("eulerians", axes, parameters),
	};
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_DEFAULTS,
		.get = hkl_mode_get_eulerians_real,
		.set = hkl_mode_set_eulerians_real,
	};

	return hkl_mode_new(&info, &operations);
};

/***********************/
/* HklEngine */
/***********************/

static void hkl_engine_eulerians_free_real(HklEngine *base)
{
	HklEngineEulerians *self;

	self = container_of(base, HklEngineEulerians, engine);
	hkl_engine_release(&self->engine);
	free(self);
}

HklEngine *hkl_engine_eulerians_new(void)
{
	HklEngineEulerians *self;
	HklMode *mode;
	static const HklPseudoAxis omega = {
		.parameter = { HKL_PARAMETER_DEFAULTS_ANGLE, .name = "omega"}
	};
	static const HklPseudoAxis chi = {
		.parameter = { HKL_PARAMETER_DEFAULTS_ANGLE, .name = "chi"}
	};
	static const HklPseudoAxis phi = {
		.parameter = {HKL_PARAMETER_DEFAULTS_ANGLE, .name = "phi"}
	};
	static const HklPseudoAxis *pseudo_axes[] = {&omega, &chi, &phi};
	static HklEngineInfo info = {
		.name = "eulerians",
		.pseudo_axes = pseudo_axes,
		.n_pseudo_axes = ARRAY_SIZE(pseudo_axes),
	};
	static HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_engine_eulerians_free_real,
	};

	self = HKL_MALLOC(HklEngineEulerians);
	hkl_engine_init(&self->engine, &info, &operations);

	/* add the pseudo axes with the new API */
	self->omega = register_pseudo_axis(&self->engine, &omega.parameter);
	self->chi = register_pseudo_axis(&self->engine, &chi.parameter);
	self->phi = register_pseudo_axis(&self->engine, &phi.parameter);

	/* eulerians [default] */
	mode = mode_eulerians();
	hkl_engine_add_mode(&self->engine, mode);
	hkl_engine_mode_set(&self->engine, mode);

	return &self->engine;
}
