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
 * Copyright (C) 2003-2016 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-pseudoaxis-common-readonly-private.h"
#include "hkl-pseudoaxis-private.h"

/* incidence */

typedef struct _HklEngineIncidence HklEngineIncidence;

struct _HklEngineIncidence
{
	HklEngine engine;
	HklParameter *incidence;
	HklParameter *azimuth;
};

#define HKL_MODE_INCIDENCE_ERROR hkl_mode_incidence_error_quark ()

static GQuark hkl_mode_incidence_error_quark (void)
{
	return g_quark_from_static_string ("hkl-mode-incidence-error-quark");
}

typedef enum {
	HKL_MODE_INCIDENCE_ERROR_GET, /* can not get the engine */
	HKL_MODE_INCIDENCE_ERROR_SET, /* can not set the engine */
} HklModeIncidenceError;

static int hkl_mode_incidence_get_real(HklMode *base,
				       HklEngine *engine,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample,
				       GError **error)
{
	HklVector ki;
	double incidence;
	double azimuth;
	static const HklVector Y = {
		.data = {0, 1, 0},
	};

	if (!base || !engine || !engine->mode || !geometry || !detector || !sample){
		g_set_error(error,
			    HKL_MODE_INCIDENCE_ERROR,
			    HKL_MODE_INCIDENCE_ERROR_GET,
			    "internal error");
		return FALSE;
	}
	HklEngineIncidence *engine_incidence = container_of(engine, HklEngineIncidence, engine);
	const HklModeIncidence *mode = container_of(base, HklModeIncidence, parent);
	HklVector n = {
		.data = {
			mode->n_x->_value,
			mode->n_y->_value,
			mode->n_z->_value,
		}
	};

	/* first check the parameters */
	if (hkl_vector_is_null(&n)){
		g_set_error(error,
			    HKL_MODE_INCIDENCE_ERROR,
			    HKL_MODE_INCIDENCE_ERROR_GET,
			    "can not compute the incidence when the surface vector is null.");
		return FALSE;
	}

	/* compute the orientation of the surface */
	hkl_vector_rotated_quaternion(&n, &darray_item(geometry->holders, 0)->q);

	hkl_source_compute_ki(&geometry->source, &ki);
	incidence = _incidence(&n, &ki);

	hkl_vector_project_on_plan(&n, &ki);
	azimuth = hkl_vector_angle(&n, &Y);

	engine_incidence->incidence->_value = incidence;
	engine_incidence->azimuth->_value = azimuth;

	return TRUE;
}

static int hkl_mode_readonly_set_real(HklMode *base,
				      HklEngine *engine,
				      HklGeometry *geometry,
				      HklDetector *detector,
				      HklSample *sample,
				      GError **error)
{
	g_set_error(error,
		    HKL_MODE_INCIDENCE_ERROR,
		    HKL_MODE_INCIDENCE_ERROR_SET,
		    "The \"%s\" engine is readonly", base->info->name);
	return FALSE;
}

HklMode *hkl_mode_incidence_new(const HklModeInfo *info)
{
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_DEFAULTS,
		.capabilities = HKL_ENGINE_CAPABILITIES_READABLE,
		.get = hkl_mode_incidence_get_real,
		.set = hkl_mode_readonly_set_real,
	};
	HklModeIncidence *self = HKL_MALLOC(HklModeIncidence);

	/* the base constructor; */
	hkl_mode_init(&self->parent,
		      info,
		      &operations, TRUE);

	self->n_x = register_mode_parameter(&self->parent, 0);
	self->n_y = register_mode_parameter(&self->parent, 1);
	self->n_z = register_mode_parameter(&self->parent, 2);

	return &self->parent;
}

HklEngine *hkl_engine_incidence_new(HklEngineList *engines)
{
	HklEngineIncidence *self;
	static const HklParameter incidence = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "incidence",
		.description = "incidence of the incomming beam.",
	};
	static const HklParameter azimuth = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "azimuth",
		.description = "azimuth of the sample surface (projection of $\\vec{n}$ on the $yOz$ plan",
	};
	static const HklParameter *pseudo_axes[] = {&incidence, &azimuth};
	static HklEngineInfo info = {
		HKL_ENGINE_INFO("incidence",
				pseudo_axes,
				HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_SAMPLE),
	};
	static HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
	};

	self = HKL_MALLOC(HklEngineIncidence);

	hkl_engine_init(&self->engine, &info, &operations, engines);

	self->incidence = register_pseudo_axis(&self->engine, engines, &incidence);
	self->azimuth = register_pseudo_axis(&self->engine, engines, &azimuth);

	return &self->engine;
}

/* emergence */

typedef struct _HklEngineEmergence HklEngineEmergence;

struct _HklEngineEmergence
{
	HklEngine engine;
	HklParameter *emergence;
	HklParameter *azimuth;
};

#define HKL_MODE_EMERGENCE_ERROR hkl_mode_emergence_error_quark ()

static GQuark hkl_mode_emergence_error_quark (void)
{
	return g_quark_from_static_string ("hkl-mode-emergence-error-quark");
}

typedef enum {
	HKL_MODE_EMERGENCE_ERROR_GET, /* can not get the engine */
	HKL_MODE_EMERGENCE_ERROR_SET, /* can not set the engine */
} HklModeEmergenceError;

static int hkl_mode_emergence_get_real(HklMode *base,
				       HklEngine *engine_base,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample,
				       GError **error)
{
	HklVector kf;
	double emergence;
	double azimuth;
	static const HklVector X = {
		.data = {1, 0, 0},
	};
	static const HklVector Y = {
		.data = {0, 1, 0},
	};

	if (!base || !engine_base || !engine_base->mode || !geometry || !detector || !sample){
		g_set_error(error,
			    HKL_MODE_EMERGENCE_ERROR,
			    HKL_MODE_EMERGENCE_ERROR_GET,
			    "internal error");
		return FALSE;
	}
	HklEngineEmergence *engine = container_of(engine_base, HklEngineEmergence, engine);
	const HklModeIncidence *mode = container_of(base, HklModeIncidence, parent);
	HklVector n = {
		.data = {
			mode->n_x->_value,
			mode->n_y->_value,
			mode->n_z->_value,
		}
	};

	/* first check the parameters */
	if (hkl_vector_is_null(&n)){
		g_set_error(error,
			    HKL_MODE_EMERGENCE_ERROR,
			    HKL_MODE_EMERGENCE_ERROR_GET,
			    "Can not compute the emergence when the surface vector is null.");
		return FALSE;
	}

	/* compute the orientation of the surface */
	hkl_vector_rotated_quaternion(&n, &darray_item(geometry->holders, 0)->q);

	hkl_detector_compute_kf(detector, geometry, &kf);
	emergence = _emergence(&n, &kf);

	hkl_vector_project_on_plan(&n, &X);
	azimuth = hkl_vector_angle(&n, &Y);

	engine->emergence->_value = emergence;
	engine->azimuth->_value = azimuth;

	return TRUE;
}

HklMode *hkl_mode_emergence_new(const HklModeInfo *info)
{
	static const HklModeOperations operations = {
		HKL_MODE_OPERATIONS_DEFAULTS,
		.capabilities = HKL_ENGINE_CAPABILITIES_READABLE,
		.get = hkl_mode_emergence_get_real,
		.set = hkl_mode_readonly_set_real,
	};
	HklModeIncidence *self = HKL_MALLOC(HklModeIncidence);

	/* the base constructor; */
	hkl_mode_init(&self->parent,
		      info,
		      &operations, TRUE);

	self->n_x = register_mode_parameter(&self->parent, 0);
	self->n_y = register_mode_parameter(&self->parent, 1);
	self->n_z = register_mode_parameter(&self->parent, 2);

	return &self->parent;
}

HklEngine *hkl_engine_emergence_new(HklEngineList *engines)
{
	HklEngineEmergence *self;
	static const HklParameter emergence = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "emergence",
		.description = "incidence of the outgoing beam.",
	};
	static const HklParameter azimuth = {
		HKL_PARAMETER_DEFAULTS_ANGLE, .name = "azimuth",
		.description = "azimuth of the sample surface (projection of $\\vec{n}$ on the $yOz$ plan",
	};
	static const HklParameter *pseudo_axes[] = {&emergence, &azimuth};
	static HklEngineInfo info = {
		HKL_ENGINE_INFO("emergence",
				pseudo_axes,
				HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_SAMPLE),
	};
	static HklEngineOperations operations = {
		HKL_ENGINE_OPERATIONS_DEFAULTS,
	};

	self = HKL_MALLOC(HklEngineEmergence);

	hkl_engine_init(&self->engine, &info, &operations, engines);

	self->emergence = register_pseudo_axis(&self->engine, engines, &emergence);
	self->azimuth = register_pseudo_axis(&self->engine, engines, &azimuth);

	return &self->engine;
}
