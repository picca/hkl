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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
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
#include "hkl-geometry-private.h"
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for _HklParameter, etc
#include "hkl-pseudoaxis-common-eulerians-private.h"
#include "hkl-pseudoaxis-private.h"     // for _HklPseudoAxis, etc
#include "hkl.h"                        // for HklParameter, HklPseudoAxis, etc
#include "hkl/ccan/array_size/array_size.h"  // for ARRAY_SIZE
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_item

typedef struct _HklModeEulerians HklModeEulerians;
typedef struct _HklEngineEulerians HklEngineEulerians;

struct _HklModeEulerians
{
	HklMode parent;
	HklParameter *solutions;
};

struct _HklEngineEulerians
{
	HklEngine engine;
	HklParameter *omega;
	HklParameter *chi;
	HklParameter *phi;
};

#define HKL_MODE_EULERIANS_ERROR hkl_mode_eulerians_error_quark ()

static GQuark hkl_mode_eulerians_error_quark (void)
{
	return g_quark_from_static_string ("hkl-mode-eulerians-error-quark");
}

typedef enum {
	HKL_MODE_EULERIANS_ERROR_SET, /* can not set the engine */
} HklModeEuleriansError;

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

	return TRUE;
}

static int eulerian_to_kappa(const double omega, const double chi, const double phi,
			     double angles[],
			     double alpha, double solution)
{
	int status = TRUE;
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
		status = FALSE;

	return status;
}

void kappa_2_kappap(double komega, double kappa, double kphi, double alpha,
		    double *komegap, double *kappap, double *kphip)
{
	double p = atan(tan(kappa/2.) * cos(alpha));
	double omega = komega + p - M_PI_2;
	double phi = kphi + p + M_PI_2;

	*komegap = gsl_sf_angle_restrict_symm(2*omega - komega);
	*kappap = -kappa;
	*kphip = gsl_sf_angle_restrict_symm(2*phi - kphi);

}

/***********/
/* HklMode */
/***********/

static int hkl_mode_get_eulerians_real(HklMode *base,
				       HklEngine *engine,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample,
				       GError **error)
{
	HklModeEulerians *self = container_of(base, HklModeEulerians, parent);
	HklEngineEulerians *eulerians = container_of(engine, HklEngineEulerians, engine);;
	const double angles[] = {
		hkl_parameter_value_get(
			hkl_geometry_get_axis_by_name(geometry, "komega"),
			HKL_UNIT_DEFAULT),
		hkl_parameter_value_get(
			hkl_geometry_get_axis_by_name(geometry, "kappa"),
			HKL_UNIT_DEFAULT),
		hkl_parameter_value_get(
			hkl_geometry_get_axis_by_name(geometry, "kphi"),
			HKL_UNIT_DEFAULT),
	};

	hkl_geometry_update(geometry);

	kappa_to_eulerian(angles,
			  &eulerians->omega->_value,
			  &eulerians->chi->_value,
			  &eulerians->phi->_value,
			  50 * HKL_DEGTORAD, self->solutions->_value);

	return TRUE;
}

static int hkl_mode_set_eulerians_real(HklMode *base,
				       HklEngine *engine,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample,
				       GError **error)
{
	HklModeEulerians *self = container_of(base, HklModeEulerians, parent);
	HklEngineEulerians *eulerians = container_of(engine, HklEngineEulerians, engine);;
	double angles[3];

	if(!eulerian_to_kappa(eulerians->omega->_value,
			      eulerians->chi->_value,
			      eulerians->phi->_value,
			      angles, 50 * HKL_DEGTORAD,
			      self->solutions->_value)){
		g_set_error(error,
			    HKL_MODE_EULERIANS_ERROR,
			    HKL_MODE_EULERIANS_ERROR_SET,
			    "unreachable solution : 0° < chi < 50°");
		return FALSE;
	}else
		hkl_engine_add_geometry(engine, angles);

	return TRUE;
}

static HklMode *mode_eulerians()
{
	static const char *axes[] = {"komega", "kappa", "kphi"};
	static const HklParameter parameters[] = {
		{ HKL_PARAMETER_DEFAULTS,.name = "solutions", ._value = 1,
		  .description = "(0/1) to select the first or second solution",
		  .range = { .max = 1 },
		},
	};
	static const HklModeInfo info = {
		HKL_MODE_INFO_WITH_PARAMS("eulerians", axes, axes, parameters),
	};
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_DEFAULTS,
		.get = hkl_mode_get_eulerians_real,
		.set = hkl_mode_set_eulerians_real,
	};

	HklModeEulerians *self = HKL_MALLOC(HklModeEulerians);

	/* the base constructor; */
	hkl_mode_init(&self->parent,
		      &info,
		      &operations, TRUE);

	self->solutions = register_mode_parameter(&self->parent, 0);

	return &self->parent;
};

/*************/
/* HklEngine */
/*************/

static void hkl_engine_eulerians_free_real(HklEngine *base)
{
	HklEngineEulerians *self;

	self = container_of(base, HklEngineEulerians, engine);
	hkl_engine_release(&self->engine);
	free(self);
}

HklEngine *hkl_engine_eulerians_new(HklEngineList *engines)
{
	HklEngineEulerians *self;
	HklMode *mode;
	static const HklParameter omega = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "omega",
		.description = "omega equivalent for a four circle eulerian geometry",
	};
	static const HklParameter chi = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "chi",
		.description = "chi equivalent for a four circle eulerian geometry",
	};
	static const HklParameter phi = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "phi",
		.description = "phi equivalent for a four circle eulerian geometry",
	};
	static const HklParameter *pseudo_axes[] = {&omega, &chi, &phi};
	static HklEngineInfo info = {
		HKL_ENGINE_INFO("eulerians",
				pseudo_axes,
				HKL_ENGINE_DEPENDENCIES_AXES),
	};
	static HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
		.free=hkl_engine_eulerians_free_real,
	};

	self = HKL_MALLOC(HklEngineEulerians);
	hkl_engine_init(&self->engine, &info, &operations, engines);

	/* add the pseudo axes with the new API */
	self->omega = register_pseudo_axis(&self->engine, engines, &omega);
	self->chi = register_pseudo_axis(&self->engine, engines, &chi);
	self->phi = register_pseudo_axis(&self->engine, engines, &phi);

	/* eulerians [default] */
	mode = mode_eulerians();
	hkl_engine_add_mode(&self->engine, mode);
	hkl_engine_mode_set(&self->engine, mode);

	return &self->engine;
}
