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

HKL_BEGIN_DECLS

struct _HklFunction
{
	const uint size;
	int (* function) (const gsl_vector *x, void *params, gsl_vector *f);
};

#define HKL_MODE_OPERATIONS_DEFAULTS .init = NULL,		\
		.get = NULL,					\
		.set = hkl_pseudo_axis_engine_mode_set_real

#define INFO(n, ax) .name = n, .axes=ax, .n_axes=ARRAY_SIZE(ax)
#define INFO_WITH_PARAMS(name, axes, parameters) INFO(name, axes), .parameters=parameters, .n_parameters=ARRAY_SIZE(parameters)

static inline void set_geometry_axes(HklPseudoAxisEngine *engine, const double values[])
{
	HklAxis *axis;
	uint i = 0;

	list_for_each(&engine->axes, axis, engine_list)
		hkl_axis_set_value(axis, values[i++]);
	hkl_geometry_update(engine->geometry);
}

/*****************/
/* HklPseudoAxis */
/*****************/

static inline void hkl_pseudo_axis_init(HklPseudoAxis *self,
				 const HklParameter *parameter,
				 HklPseudoAxisEngine *engine)
{
	self->parent = *parameter;
	self->engine = engine;
}

static inline HklPseudoAxis *hkl_pseudo_axis_new(const HklParameter *parameter,
					  HklPseudoAxisEngine *engine)
{
	HklPseudoAxis *self;

	self = HKL_MALLOC(HklPseudoAxis);

	hkl_pseudo_axis_init(self, parameter, engine);

	return self;
}

static inline HklPseudoAxis *hkl_pseudo_axis_dup(const HklPseudoAxis *self)
{
	HklPseudoAxis *dup;

	if(!self)
		return NULL;

	dup = HKL_MALLOC(HklPseudoAxis);

	dup->parent = self->parent;
	dup->engine = self->engine;

	return dup;
}

static inline void hkl_pseudo_axis_free(HklPseudoAxis *self)
{
	if(self)
		free(self);
}

/***************************/
/* HklPseudoAxisEngineMode */
/***************************/

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

	for(i=0; i<self->info->n_functions; ++i)
		hkl_assert(self->info->functions[i]->size == self->info->n_axes);

	/* parameters */
	self->parameters = realloc(self->parameters, sizeof(*self->parameters) * self->info->n_parameters);
	self->parameters_len = self->info->n_parameters;
	memcpy(self->parameters, self->info->parameters, sizeof(*self->parameters) * self->parameters_len);

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

	self->parameters = NULL;
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
	free(self->parameters);
	self->parameters_len = 0;

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

static inline HklPseudoAxisEngine *hkl_pseudo_axis_engine_new(const HklPseudoAxisEngineInfo *info)
{
	size_t i;
	HklPseudoAxisEngine *self = NULL;

	self = HKL_MALLOC(HklPseudoAxisEngine);

	self->info = info;
	list_head_init(&self->modes);

	/* create the pseudoAxes */
	list_head_init(&self->pseudo_axes);
	for(i=0; i<self->info->n_pseudo_axes; ++i){
		HklPseudoAxis *pseudo_axis;

		pseudo_axis =  hkl_pseudo_axis_new(&info->pseudo_axes[i]->parent, self);
		list_add_tail(&self->pseudo_axes, &pseudo_axis->list);
	}

	return self;
}

/**
 * hkl_pseudo_axis_engine_free: (skip)
 * @self: the engine to release
 *
 * release the memory of an HklPseudoAxisEngine
 **/
static inline void hkl_pseudo_axis_engine_free(HklPseudoAxisEngine *self)
{
	size_t i;
	HklPseudoAxisEngineMode *mode;
	HklPseudoAxisEngineMode *next;
	HklPseudoAxis *pseudo_axis;
	HklPseudoAxis *pseudo_axis_next;

	if (self->geometry)
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
	list_for_each_safe(&self->pseudo_axes, pseudo_axis, pseudo_axis_next, list){
		list_del(&pseudo_axis->list);
		hkl_pseudo_axis_free(pseudo_axis);
	}

	free(self);
}

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
		hkl_axis_set_value(axis, gsl_sf_angle_restrict_symm(x[i++]));
	}

	hkl_geometry_list_add(self->engines->geometries, self->geometry);
}

/***************************/
/* HklPseudoAxisEngineList */
/***************************/

extern HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new(void);

extern const HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new_copy(const HklPseudoAxisEngineList *self);

extern int hkl_pseudo_axis_engine_list_add(HklPseudoAxisEngineList *self,
					   HklPseudoAxisEngine *engine);

extern void hkl_pseudo_axis_engine_list_clear(HklPseudoAxisEngineList *self);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_PRIVATE_H__ */
