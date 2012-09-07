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
 */
#ifndef __HKL_PSEUDOAXIS_PRIVATE_H__
#define __HKL_PSEUDOAXIS_PRIVATE_H__

#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>
#include <hkl/hkl-parameter-private.h>

HKL_BEGIN_DECLS

#define INFO(n, ax) .name = n, .axes=ax, .n_axes=ARRAY_SIZE(ax)
#define INFO_WITH_PARAMS(name, axes, parameters) INFO(name, axes), .parameters=parameters, .n_parameters=ARRAY_SIZE(parameters)

static inline void set_geometry_axes(HklPseudoAxisEngine *engine, const double values[])
{
	HklAxis *axis;
	uint i = 0;

	list_for_each(&engine->axes, axis, engine_list)
		hkl_parameter_set_value(&axis->parameter, values[i++], NULL);
	hkl_geometry_update(engine->geometry);
}

/*****************/
/* HklPseudoAxis */
/*****************/

struct _HklPseudoAxis
{
	HklParameter parameter;
	HklPseudoAxisEngine *engine;
};

extern HklParameter *hkl_parameter_new_pseudo_axis(
	const HklParameter *parameter,
	HklPseudoAxisEngine *engine);

/***************************/
/* HklPseudoAxisEngineMode */
/***************************/

struct _HklPseudoAxisEngineModeOperations
{
	int (* init)(HklPseudoAxisEngineMode *self,
		     HklPseudoAxisEngine *engine,
		     HklGeometry *geometry,
		     HklDetector *detector,
		     HklSample *sample,
		     HklError **error);
	int (* get)(HklPseudoAxisEngineMode *self,
		    HklPseudoAxisEngine *engine,
		    HklGeometry *geometry,
		    HklDetector *detector,
		    HklSample *sample,
		    HklError **error);
	int (* set)(HklPseudoAxisEngineMode *self,
		    HklPseudoAxisEngine *engine,
		    HklGeometry *geometry,
		    HklDetector *detector,
		    HklSample *sample,
		    HklError **error);
};

#define HKL_PSEUDO_AXIS_ENGINE_MODE_OPERATIONS_DEFAULTS		\
	.init=hkl_pseudo_axis_engine_mode_init_real,		\
		.get=hkl_pseudo_axis_engine_mode_get_real,	\
		.set=hkl_pseudo_axis_engine_mode_set_real


static int hkl_pseudo_axis_engine_mode_init_real(HklPseudoAxisEngineMode *mode,
						 HklPseudoAxisEngine *self,
						 HklGeometry *geometry,
						 HklDetector *detector,
						 HklSample *sample,
						 HklError **error)
{
	if (!self || !mode || !geometry || !detector || !sample)
		return HKL_FALSE;

	/* update the geometry internals */
	hkl_geometry_update(geometry);

	if(mode->geometry_init)
		hkl_geometry_free(mode->geometry_init);
	mode->geometry_init = hkl_geometry_new_copy(geometry);

	if(mode->detector_init)
		hkl_detector_free(mode->detector_init);
	mode->detector_init = hkl_detector_new_copy(detector);

	if(mode->sample_init)
		hkl_sample_free(mode->sample_init);
	mode->sample_init = hkl_sample_new_copy(sample);

	return HKL_TRUE;
}

static int hkl_pseudo_axis_engine_mode_get_real(HklPseudoAxisEngineMode *self,
						HklPseudoAxisEngine *engine,
						HklGeometry *geometry,
						HklDetector *detector,
						HklSample *sample,
						HklError **error)
{
}

static int hkl_pseudo_axis_engine_mode_set_real(HklPseudoAxisEngineMode *self,
						HklPseudoAxisEngine *engine,
						HklGeometry *geometry,
						HklDetector *detector,
						HklSample *sample,
						HklError **error)
{
}

static inline int hkl_pseudo_axis_engine_mode_init(
	HklPseudoAxisEngineMode *self,
	const HklPseudoAxisEngineModeInfo *info,
	const HklPseudoAxisEngineModeOperations *op)
{
	size_t i;

	/* ensure part */
	if (!self)
		return HKL_FALSE;

	self->info = info;
	self->op = op;

	/* parameters */
	hkl_parameter_list_init(&self->parameters,
				&hkl_parameter_list_operations_defaults);
	for(i=0; i<self->info->n_parameters; ++i){
		HklParameter *parameter;

		parameter = hkl_parameter_new_copy(&self->info->parameters[i]);
		hkl_parameter_list_add_parameter(&self->parameters, parameter);
	}

	/* init part */
	self->geometry_init = NULL;
	self->detector_init = NULL;
	self->sample_init = NULL;

	return HKL_TRUE;
}

static inline HklPseudoAxisEngineMode *hkl_pseudo_axis_engine_mode_new(
	const HklPseudoAxisEngineModeInfo *info,
	const HklPseudoAxisEngineModeOperations *op)
{
	HklPseudoAxisEngineMode *self = NULL;


	self = HKL_MALLOC(HklPseudoAxisEngineMode);

	hkl_pseudo_axis_engine_mode_init(self, info, op);

	return self;
}

/**
 * hkl_pseudo_axis_engine_mode_free: (skip)
 * @self:
 *
 * delete an HklPseudoAxisEngineMode
 **/
static inline void hkl_pseudo_axis_engine_mode_free(HklPseudoAxisEngineMode *self)
{
	hkl_parameter_list_release(&self->parameters);

	if(self->geometry_init){
		hkl_geometry_free(self->geometry_init);
		self->geometry_init = NULL;
	}
	if(self->detector_init){
		hkl_detector_free(self->detector_init);
		self->detector_init = NULL;
	}
	if(self->sample_init){
		hkl_sample_free(self->sample_init);
		self->sample_init = NULL;
	}
	free(self);
}

/***********************/
/* HklPseudoAxisEngine */
/***********************/

static void hkl_pseudo_axis_engine_release(HklPseudoAxisEngine *self)
{
	HklPseudoAxisEngineMode *mode;
	HklPseudoAxisEngineMode *next;

	if(self->geometry)
		hkl_geometry_free(self->geometry);

	if(self->detector)
		hkl_detector_free(self->detector);

	if(self->sample)
		hkl_sample_free(self->sample);

	/* release the mode added */
	list_for_each_safe(&self->modes, mode, next, list){
		list_del(&mode->list);
		hkl_pseudo_axis_engine_mode_free(mode);
	}
	self->mode = NULL;

	/* release the HklPseudoAxe memory */
	hkl_parameter_list_release(&self->pseudo_axes);
}

struct _HklPseudoAxisEngineOperations
{
	void (*free)(HklPseudoAxisEngine *self);
};

#define HKL_PSEUDO_AXIS_ENGINE_OPERATIONS_DEFAULTS	\
	.free=hkl_pseudo_axis_engine_free_real

static inline void hkl_pseudo_axis_engine_free_real(HklPseudoAxisEngine *self)
{
}

static void hkl_pseudo_axis_engine_free(HklPseudoAxisEngine *self)
{
	self->ops->free(self);
}

extern void hkl_pseudo_axis_engine_init(HklPseudoAxisEngine *engine,
					const HklPseudoAxisEngineInfo *info,
					const HklPseudoAxisEngineOperations *ops);


extern void unregister_pseudo_axis(HklParameter *pseudo_axis);

extern HklParameter *register_pseudo_axis(HklPseudoAxisEngine *self,
					  const HklParameter *parameter);

/**
 * hkl_pseudo_axis_engine_add_mode: (skip)
 * @self:
 * @mode: the mode to add
 *
 * add an HklPseudoAxisEngineMode to the self HklPseudoAxisEngine
 **/
static inline void hkl_pseudo_axis_engine_add_mode(HklPseudoAxisEngine *self,
						   HklPseudoAxisEngineMode *mode)
{
	list_add_tail(&self->modes, &mode->list);
}

/**
 * hkl_pseudo_axis_engine_add_geometry: (skip)
 * @self: the current PseudoAxeEngine
 * @x: x A vector of double with the axes values to put in the geometry.
 *
 * This method try to be clever by allocating memory only if the
 * current length of the geometries is not large enought. Then it just
 * set the geometry axes and copy it to the right geometries. We do
 * not gives the x len as it is equal to the self->axes_len.
 *
 **/
static inline void hkl_pseudo_axis_engine_add_geometry(HklPseudoAxisEngine *self,
						       double const x[])
{
	HklAxis *axis;
	uint i = 0;

	/* copy the axes configuration into the engine->geometry */
	list_for_each(&self->axes, axis, engine_list){
		hkl_parameter_set_value(&axis->parameter,
					gsl_sf_angle_restrict_symm(x[i++]),
					NULL);
	}

	hkl_geometry_list_add(self->engines->geometries, self->geometry);
}

/***************************/
/* HklEngineList */
/***************************/

extern HklEngineList *hkl_engine_list_new(void);

extern const HklEngineList *hkl_engine_list_new_copy(const HklEngineList *self);

extern int hkl_engine_list_add(HklEngineList *self,
					   HklPseudoAxisEngine *engine);

extern void hkl_engine_list_clear(HklEngineList *self);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_PRIVATE_H__ */
