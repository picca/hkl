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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
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
#include "hkl-geometry-private.h"       // for hkl_geometry_update, etc
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for hkl_parameter_list_free, etc
#include "hkl.h"                        // for HklEngine, HklMode, etc
#include "hkl/ccan/darray/darray.h"     // for darray_foreach, etc

HKL_BEGIN_DECLS

typedef struct _HklModeOperations HklModeOperations;
typedef struct _HklModeInfo HklModeInfo;
typedef struct _HklMode HklMode;
typedef struct _HklEngineInfo HklEngineInfo;
typedef struct _HklEngineOperations HklEngineOperations;

typedef darray(HklMode *) darray_mode;

struct _HklModeInfo {
	const char *name;
	const char **axes;
	uint n_axes;
	const HklParameter *parameters;
	uint n_parameters;
};

struct _HklMode
{
	const HklModeInfo *info;
	const HklModeOperations *ops;
	darray_parameter parameters;
	darray_string parameters_names;
};

struct _HklEngineInfo {
	const char *name;
	const HklPseudoAxis **pseudo_axes;
	uint n_pseudo_axes;
};

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
	darray_string pseudo_axes_names;
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
};

#define INFO(n, ax) .name = n, .axes=ax, .n_axes=ARRAY_SIZE(ax)
#define INFO_WITH_PARAMS(name, axes, parameters) INFO(name, axes), .parameters=parameters, .n_parameters=ARRAY_SIZE(parameters)

static inline void set_geometry_axes(HklEngine *engine, const double values[])
{
	HklParameter **axis;
	uint i = 0;

	darray_foreach(axis, engine->axes){
		hkl_parameter_value_set(*axis, values[i++], NULL);
	}
	hkl_geometry_update(engine->geometry);
}

/*****************/
/* HklPseudoAxis */
/*****************/

struct _HklPseudoAxis
{
	HklParameter parameter;
	HklEngine *engine;
};

extern HklParameter *hkl_parameter_new_pseudo_axis(
	const HklParameter *parameter,
	HklEngine *engine);

/***********/
/* HklMode */
/***********/

struct _HklModeOperations
{
	void (* free)(HklMode *self);
	int (* init)(HklMode *self,
		     HklEngine *engine,
		     HklGeometry *geometry,
		     HklDetector *detector,
		     HklSample *sample,
		     HklError **error);
	int (* get)(HklMode *self,
		    HklEngine *engine,
		    HklGeometry *geometry,
		    HklDetector *detector,
		    HklSample *sample,
		    HklError **error);
	int (* set)(HklMode *self,
		    HklEngine *engine,
		    HklGeometry *geometry,
		    HklDetector *detector,
		    HklSample *sample,
		    HklError **error);
};

#define HKL_MODE_OPERATIONS_DEFAULTS		\
	.free=hkl_mode_free_real,		\
		.init=hkl_mode_init_real,	\
		.get=hkl_mode_get_real,		\
		.set=hkl_mode_set_real

static inline void hkl_mode_free_real(HklMode *self)
{
	hkl_parameter_list_free(&self->parameters);
	darray_free(self->parameters_names);

	free(self);
}

static int hkl_mode_init_real(HklMode *mode,
			      HklEngine *self,
			      HklGeometry *geometry,
			      HklDetector *detector,
			      HklSample *sample,
			      HklError **error)
{
	if (!self || !mode || !geometry || !detector || !sample)
		return HKL_FALSE;

	/* update the geometry internals */
	hkl_geometry_update(geometry);

	return HKL_TRUE;
}

static int hkl_mode_get_real(HklMode *self,
			     HklEngine *engine,
			     HklGeometry *geometry,
			     HklDetector *detector,
			     HklSample *sample,
			     HklError **error)
{
	return HKL_TRUE;
}

static int hkl_mode_set_real(HklMode *self,
			     HklEngine *engine,
			     HklGeometry *geometry,
			     HklDetector *detector,
			     HklSample *sample,
			     HklError **error)
{
	return HKL_TRUE;
}

static inline int hkl_mode_init(
	HklMode *self,
	const HklModeInfo *info,
	const HklModeOperations *ops)
{
	size_t i;

	/* ensure part */
	if (!self)
		return HKL_FALSE;

	self->info = info;
	self->ops = ops;

	/* parameters */
	darray_init(self->parameters);
	darray_init(self->parameters_names);
	for(i=0; i<self->info->n_parameters; ++i){
		HklParameter *parameter;

		parameter = hkl_parameter_new_copy(&self->info->parameters[i]);
		darray_append(self->parameters, parameter);
		darray_append(self->parameters_names, parameter->name);
	}

	return HKL_TRUE;
}

static inline HklMode *hkl_mode_new(
	const HklModeInfo *info,
	const HklModeOperations *op)
{
	HklMode *self = NULL;


	self = HKL_MALLOC(HklMode);

	hkl_mode_init(self, info, op);

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

static void hkl_engine_release(HklEngine *self)
{
	HklMode **mode;

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

	/* release the HklPseudoAxe memory */
	hkl_parameter_list_free(&self->pseudo_axes);

	darray_free(self->pseudo_axes_names);

	darray_free(self->mode_names);
}

struct _HklEngineOperations
{
	void (*free)(HklEngine *self);
};

#define HKL_ENGINE_OPERATIONS_DEFAULTS		\
	.free=hkl_engine_free_real

static inline void hkl_engine_free_real(HklEngine *self)
{
}

static void hkl_engine_free(HklEngine *self)
{
	self->ops->free(self);
}

extern void hkl_engine_init(HklEngine *engine,
			    const HklEngineInfo *info,
			    const HklEngineOperations *ops);


extern void unregister_pseudo_axis(HklParameter *pseudo_axis);

extern HklParameter *register_pseudo_axis(HklEngine *self,
					  const HklParameter *parameter);

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
					NULL);
	}

	hkl_geometry_list_add(self->engines->geometries, self->geometry);
}

extern void hkl_engine_mode_set(HklEngine *self, HklMode *mode);

/*****************/
/* HklEngineList */
/*****************/

extern HklEngineList *hkl_engine_list_new(void);

extern const HklEngineList *hkl_engine_list_new_copy(const HklEngineList *self);

extern int hkl_engine_list_add(HklEngineList *self,
			       HklEngine *engine);

extern void hkl_engine_list_clear(HklEngineList *self);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_PRIVATE_H__ */
