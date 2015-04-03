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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PSEUDOAXIS_PRIVATE_H__
#define __HKL_PSEUDOAXIS_PRIVATE_H__

#include <gsl/gsl_sf_trig.h>            // for gsl_sf_angle_restrict_symm
#include <stddef.h>                     // for size_t
#include <stdlib.h>                     // for free
#include <string.h>                     // for NULL
#include <sys/types.h>                  // for uint
#include "hkl-detector-private.h"
#include "hkl-geometry-private.h"       // for hkl_geometry_update, etc
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for hkl_parameter_list_free, etc
#include "hkl.h"                        // for HklEngine, HklMode, etc
#include "hkl/ccan/darray/darray.h"     // for darray_foreach, etc

G_BEGIN_DECLS

typedef struct _HklModeOperations HklModeOperations;
typedef struct _HklModeInfo HklModeInfo;
typedef struct _HklMode HklMode;
typedef struct _HklEngineInfo HklEngineInfo;
typedef struct _HklEngineOperations HklEngineOperations;

typedef darray(HklMode *) darray_mode;

/***********/
/* HklMode */
/***********/

struct _HklModeInfo {
	const char *name;
	const darray_string axes_r;
	const darray_string axes_w;
	const darray(const HklParameter) parameters;
};

#define HKL_MODE_INFO(_name, _axes_r, _axes_w) .name=_name, .axes_r=DARRAY(_axes_r), .axes_w=DARRAY(_axes_w)

#define HKL_MODE_INFO_WITH_PARAMS(_name, _axes_r, _axes_w, _parameters)	\
	HKL_MODE_INFO(_name, _axes_r, _axes_w), .parameters=DARRAY(_parameters)

struct _HklModeOperations
{
	unsigned long capabilities;

	void (* free)(HklMode *self);
	int (* initialized_get)(const HklMode *self);
	int (* initialized_set)(HklMode *self,
				HklEngine *engine,
				HklGeometry *geometry,
				HklDetector *detector,
				HklSample *sample,
				int initialized,
				GError **error);
	int (* get)(HklMode *self,
		    HklEngine *engine,
		    HklGeometry *geometry,
		    HklDetector *detector,
		    HklSample *sample,
		    GError **error);
	int (* set)(HklMode *self,
		    HklEngine *engine,
		    HklGeometry *geometry,
		    HklDetector *detector,
		    HklSample *sample,
		    GError **error);
};


#define HKL_MODE_OPERATIONS_DEFAULTS .capabilities=HKL_ENGINE_CAPABILITIES_READABLE | HKL_ENGINE_CAPABILITIES_WRITABLE, \
		.free=hkl_mode_free_real,				\
		.initialized_get=hkl_mode_initialized_get_real,		\
		.initialized_set=hkl_mode_initialized_set_real,		\
		.get=hkl_mode_get_real,					\
		.set=hkl_mode_set_real


struct _HklMode
{
	const HklModeInfo *info;
	const HklModeOperations *ops;
	darray_parameter parameters;
	darray_string parameters_names;
	int initialized;
};


static inline void hkl_mode_free_real(HklMode *self)
{
	HklParameter **parameter;

	darray_foreach(parameter, self->parameters){
		hkl_parameter_free(*parameter);
	}
	darray_free(self->parameters);

	darray_free(self->parameters_names);

	free(self);
}


static inline int hkl_mode_initialized_get_real(const HklMode *self)
{
	return self->initialized;
}


static inline int hkl_mode_initialized_get(const HklMode *self)
{
	return self->ops->initialized_get(self);
}


static inline int hkl_mode_initialized_set_real(HklMode *self,
						HklEngine *engine,
						HklGeometry *geometry,
						HklDetector *detector,
						HklSample *sample,
						int initialized,
						GError **error)
{
	/* by default do nothing and no error */
	return TRUE;
}


static inline int hkl_mode_initialized_set(HklMode *self,
					   HklEngine *engine,
					   HklGeometry *geometry,
					   HklDetector *detector,
					   HklSample *sample,
					   int initialized,
					   GError **error)
{
	return self->ops->initialized_set(self,
					  engine, geometry, detector, sample,
					  initialized, error);
}


static inline int hkl_mode_get_real(HklMode *self,
				    HklEngine *engine,
				    HklGeometry *geometry,
				    HklDetector *detector,
				    HklSample *sample,
				    GError **error)
{
	/* by default do nothing and no error */
	return TRUE;
}


static inline int hkl_mode_set_real(HklMode *self,
				    HklEngine *engine,
				    HklGeometry *geometry,
				    HklDetector *detector,
				    HklSample *sample,
				    GError **error)
{
	/* by default do nothing and no error */
	return TRUE;
}


static inline int hkl_mode_init(HklMode *self,
				const HklModeInfo *info,
				const HklModeOperations *ops,
				int initialized)
{
	size_t i;
	const HklParameter *parameter;

	/* ensure part */
	if (!self)
		return FALSE;

	self->info = info;
	self->ops = ops;

	/* parameters */
	darray_init(self->parameters);
	darray_init(self->parameters_names);
	darray_foreach(parameter, self->info->parameters){
		darray_append(self->parameters, hkl_parameter_new_copy(parameter));
		darray_append(self->parameters_names, parameter->name);
	}

	self->initialized = initialized;

	return TRUE;
}


static inline HklMode *hkl_mode_new(const HklModeInfo *info,
				    const HklModeOperations *op,
				    int initialized)
{
	HklMode *self = NULL;

	self = HKL_MALLOC(HklMode);

	hkl_mode_init(self, info, op, initialized);

	return self;
}


/**
 * hkl_mode_free: (skip)
 * @self:
 *
 * delete an HklMode
 **/
static inline void hkl_mode_free(HklMode *self)
{
	self->ops->free(self);
}

/*************/
/* HklEngine */
/*************/


struct _HklEngineInfo {
	const char *name;
	const darray(const HklParameter *) pseudo_axes;
	unsigned int dependencies;
};

#define HKL_ENGINE_INFO(_name, _pseudo_axes, _dependencies) .name = (_name), \
		.pseudo_axes = DARRAY(_pseudo_axes),			\
		.dependencies = (_dependencies)

struct _HklEngine
{
	const HklEngineInfo *info;
	const HklEngineOperations *ops;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HklMode *mode; /* not owned */
	HklEngineList *engines; /* not owned */
	darray_parameter axes;
	darray_parameter pseudo_axes;
	darray_string pseudo_axis_names;
	darray_mode modes;
	darray_string mode_names;
};


struct _HklEngineList
{
	_darray(HklEngine *);
	HklGeometryList *geometries;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	darray_parameter pseudo_axes;
};


#define HKL_ENGINE_ERROR hkl_engine_error_quark ()


static GQuark hkl_engine_error_quark (void)
{
	return g_quark_from_static_string ("hkl-engine-error-quark");
}


typedef enum {
	HKL_ENGINE_ERROR_PSEUDO_AXIS_VALUES_GET, /* can not get the engine pseudo axes values */
	HKL_ENGINE_ERROR_PSEUDO_AXIS_VALUES_SET, /* can not set the engine pseudo axes values */
	HKL_ENGINE_ERROR_PSEUDO_AXIS_SET, /* can not set the pseudo axis */
	HKL_ENGINE_ERROR_INITIALIZE, /* can not initialize the engine */
	HKL_ENGINE_ERROR_SET, /* can not set the engine */
	HKL_ENGINE_ERROR_GET, /* can not get the engine */
	HKL_ENGINE_ERROR_PARAMETER_GET, /* can not get the parameter */
	HKL_ENGINE_ERROR_PARAMETER_SET, /* can not set the parameter */
	HKL_ENGINE_ERROR_CURRENT_MODE_SET, /* can not select the mode */
} HklEngineError;


static inline void set_geometry_axes(HklEngine *engine, const double values[])
{
	HklParameter **axis;
	uint i = 0;

	darray_foreach(axis, engine->axes){
		hkl_parameter_value_set(*axis, values[i++], HKL_UNIT_DEFAULT, NULL);
	}
	hkl_geometry_update(engine->geometry);
}


static inline void hkl_engine_release(HklEngine *self)
{
	HklMode **mode;
	HklParameter **pseudo_axis;

	if(self->geometry)
		hkl_geometry_free(self->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);

	if(self->sample)
		hkl_sample_free(self->sample);

	/* release the mode added */
	darray_foreach(mode, self->modes){
		hkl_mode_free(*mode);
	}
	darray_free(self->modes);

	darray_free(self->axes);
	darray_free(self->pseudo_axes);
	darray_free(self->pseudo_axis_names);
	darray_free(self->mode_names);
}


struct _HklEngineOperations
{
	void (*free)(HklEngine *self);
};


#define HKL_ENGINE_OPERATIONS_DEFAULTS .free=hkl_engine_free_real


static inline void hkl_engine_free_real(HklEngine *self)
{
}


static inline void hkl_engine_free(HklEngine *self)
{
	self->ops->free(self);
}


static inline void hkl_engine_init(HklEngine *self,
				   const HklEngineInfo *info,
				   const HklEngineOperations *ops,
				   HklEngineList *engines)
{
	self->info = info;
	self->ops = ops;
	darray_init(self->modes);
	darray_init(self->pseudo_axes);
	darray_init(self->pseudo_axis_names);
	darray_init(self->mode_names);
	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;
	self->engines = engines;

	darray_append(*engines, self);
}


static inline HklParameter *register_pseudo_axis(HklEngine *self,
						 HklEngineList *engines,
						 const HklParameter *pseudo_axis)
{
	HklParameter *parameter;

	/* TODO find an already existing pseudo axis in the list */

	parameter = hkl_parameter_new_copy(pseudo_axis);
	darray_append(engines->pseudo_axes, parameter);
	darray_append(self->pseudo_axes, parameter);
	darray_append(self->pseudo_axis_names, parameter->name);
	
	return parameter;
}

/**
 * hkl_engine_add_mode: (skip)
 * @self:
 * @mode: the mode to add
 *
 * add an HklMode to the self HklEngine
 **/
static inline void hkl_engine_add_mode(HklEngine *self,
				       HklMode *mode)
{
	darray_append(self->modes, mode);
	darray_append(self->mode_names, mode->info->name);
}

/**
 * hkl_engine_add_geometry: (skip)
 * @self: the current PseudoAxeEngine
 * @x: x A vector of double with the axes values to put in the geometry.
 *
 * This method try to be clever by allocating memory only if the
 * current length of the geometries is not large enought. Then it just
 * set the geometry axes and copy it to the right geometries. We do
 * not gives the x len as it is equal to the self->axes_len.
 *
 **/
static inline void hkl_engine_add_geometry(HklEngine *self,
					   double const x[])
{
	HklParameter **axis;
	uint i = 0;

	/* copy the axes configuration into the engine->geometry */
	darray_foreach(axis, self->axes){
		hkl_parameter_value_set(*axis,
					gsl_sf_angle_restrict_symm(x[i++]),
					HKL_UNIT_DEFAULT,
					NULL);
	}

	hkl_geometry_list_add(self->engines->geometries, self->geometry);
}


static inline void hkl_engine_prepare_internal(HklEngine *self)
{
	uint i;

	if(!self || !self->engines)
		return;

	/* set */
	if(self->geometry)
		hkl_geometry_free(self->geometry);
	self->geometry = hkl_geometry_new_copy(self->engines->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);
	self->detector = hkl_detector_new_copy(self->engines->detector);

	if(self->sample)
		hkl_sample_free(self->sample);
	self->sample = hkl_sample_new_copy(self->engines->sample);

	/* fill the axes member from the function */
	if(self->mode){
		const char **axis_name;

		darray_free(self->axes);
		darray_init(self->axes);
		darray_foreach(axis_name, self->mode->info->axes_w){
			HklParameter *axis = hkl_geometry_get_axis_by_name(self->geometry,
									   *axis_name);
			darray_append(self->axes, axis);
		}
	}

	/* reset the geometries len */
	hkl_geometry_list_reset(self->engines->geometries);
}

/**
 * hkl_engine_mode_set: (skip)
 * @self: the HklEngine
 * @name: the mode to select
 *
 * This method also populate the self->axes from the mode->axis_names.
 * this is to speed the computation of the numerical axes.
 **/
static inline void hkl_engine_mode_set(HklEngine *self, HklMode *mode)
{
	self->mode = mode;
}


static inline int hkl_engine_get(HklEngine *self,
				 GError **error) HKL_ARG_NONNULL(1);
/**
 * hkl_engine_get: (skip)
 * @self: The HklEngine
 * @error: return location for a GError, or NULL
 *
 * get the values of the pseudo-axes from the real-axes values
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
static inline int hkl_engine_get(HklEngine *self, GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if(!self->engines || !self->engines->geometry || !self->engines->detector
	   || !self->engines->sample || !self->mode || !self->mode->ops->get){
		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_GET,
			    "Internal error");
		return FALSE;
	}

	if (!self->mode->ops->get(self->mode,
				  self,
				  self->engines->geometry,
				  self->engines->detector,
				  self->engines->sample,
				  error)){
		hkl_assert(error == NULL || *error != NULL);
		return FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	return TRUE;
}


/**
 * hkl_engine_set: (skip)
 * @self: the HklEngine
 * @error: return location for a GError, or NULL
 *
 * use the HklPseudoaxisEngine values to compute the real axes values.
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
static inline int hkl_engine_set(HklEngine *self, GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if(!self->geometry || !self->detector || !self->sample
	   || !self->mode || !self->mode->ops->set){
		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_SET,
			    "Internal error");
		return FALSE;
	}

	hkl_engine_prepare_internal(self);

	if (!self->mode->ops->set(self->mode, self,
				  self->geometry,
				  self->detector,
				  self->sample,
				  error)){
		hkl_assert(error == NULL || *error != NULL);
		return FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	hkl_geometry_list_multiply(self->engines->geometries);
	hkl_geometry_list_multiply_from_range(self->engines->geometries);
	hkl_geometry_list_remove_invalid(self->engines->geometries);
	hkl_geometry_list_sort(self->engines->geometries, self->engines->geometry);

	if(self->engines->geometries->n_items == 0){
		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_SET,
			    "no remaining solutions");
		return FALSE;
	}

	return TRUE;
}

/* HklEngineList */


#define HKL_ENGINE_LIST_ERROR hkl_engine_list_error_quark ()


static GQuark hkl_engine_list_error_quark (void)
{
	return g_quark_from_static_string ("hkl-engine-list-error-quark");
}


typedef enum {
	HKL_ENGINE_LIST_ERROR_ENGINE_GET_BY_NAME, /* can not set this geometry */
	HKL_ENGINE_LIST_ERROR_PSEUDO_AXIS_GET_BY_NAME, /* can not set this geometry */
} HklEngineListError;


/**
 * hkl_engine_list_new: (skip)
 *
 * default constructor
 *
 * Returns:
 **/
static inline HklEngineList *hkl_engine_list_new(void)
{
	HklEngineList *self = NULL;

	self = HKL_MALLOC(HklEngineList);

	darray_init(*self);

	self->geometries = hkl_geometry_list_new();

	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;

	darray_init(self->pseudo_axes);

	return self;
}


/**
 * hkl_engine_list_new_copy: (skip)
 * @self:
 *
 * dummy copy constructor for the binding
 *
 * Returns: (transfer none): NULL all the time the structure is non-copyable
 **/
static inline const HklEngineList *hkl_engine_list_new_copy(const HklEngineList *self)
{
	return NULL;
}

/**
 * hkl_engine_list_clear: (skip)
 * @self: the engine list to clear
 *
 * remove all engine from the engine list
 **/
static inline void hkl_engine_list_clear(HklEngineList *self)
{
	HklEngine **engine;
	HklParameter **parameter;

	darray_foreach(engine, *self){
		hkl_engine_free(*engine);
	}
	darray_free(*self);

	darray_foreach(parameter, self->pseudo_axes){
		hkl_parameter_free(*parameter);
	}
	darray_free(self->pseudo_axes);
}


G_END_DECLS

#endif /* __HKL_PSEUDOAXIS_PRIVATE_H__ */
