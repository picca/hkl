/*
 * This file is part of the hkl library.
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
#include <string.h>
#ifndef _MSC_VER
# include <alloca.h>
#endif
#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis-private.h>

/*****************/
/* HklPseudoAxis */
/*****************/

static inline HklParameter *hkl_pseudo_axis_copy_real(const HklParameter *base)
{
	HklPseudoAxis *self = container_of(base, HklPseudoAxis, parameter);
	HklPseudoAxis *dup = HKL_MALLOC(HklPseudoAxis);

	*dup = *self;

	return &dup->parameter;
}

static inline void hkl_pseudo_axis_free_real(HklParameter *self)
{
	HklPseudoAxis *pseudo_axis = container_of(self, HklPseudoAxis, parameter);

	free(pseudo_axis);
}

static inline void hkl_pseudo_axis_fprintf_real(FILE *f, const HklParameter *self)
{
	HklPseudoAxis *pseudo_axis = container_of(self, HklPseudoAxis, parameter);

	hkl_parameter_fprintf_real(f, self);
	fprintf(f, " %p", pseudo_axis->engine);
}

static HklParameterOperations hkl_parameter_operations_pseudo_axis = {
	HKL_PARAMETER_OPERATIONS_DEFAULTS,
	.copy = hkl_pseudo_axis_copy_real,
	.free = hkl_pseudo_axis_free_real,
	.fprintf = hkl_pseudo_axis_fprintf_real,
};

HklParameter *hkl_parameter_new_pseudo_axis(
	const HklParameter *parameter,
	HklPseudoAxisEngine *engine)
{
	HklPseudoAxis *self;

	self = HKL_MALLOC(HklPseudoAxis);

	self->parameter = *parameter;
	self->parameter.ops = &hkl_parameter_operations_pseudo_axis;
	self->engine = engine;

	return &self->parameter;
}

/*****************************/
/* HklMode */
/*****************************/

/**
 * hkl_mode_fprintf: (skip)
 * @f:
 * @self:
 *
 * print to a FILE the HklPSeudoAxisEngineMode members
 **/
void hkl_mode_fprintf(FILE *f, const HklMode *self)
{
	unsigned int i;

	fprintf(f, "mode: \"%s\"\n", self->info->name);
	fprintf(f, "initialize: %p\n", self->op->init);
	fprintf(f, "get: %p\n", self->op->get);
	fprintf(f, "set: %p\n", self->op->set);
	hkl_parameter_list_fprintf(f, &self->parameters);
	if(self->info->axes){
		fprintf(f, "axes names:");
		for(i=0; i<self->info->n_axes; ++i)
			fprintf(f, " %s", self->info->axes[i]);
		fprintf(f, "\n");
	}
	hkl_geometry_fprintf(f, self->geometry_init);
	hkl_detector_fprintf(f, self->detector_init);
	hkl_sample_fprintf(f, self->sample_init);
}

/***********************/
/* HklPseudoAxisEngine */
/***********************/

void hkl_pseudo_axis_engine_init(HklPseudoAxisEngine *self,
				 const HklPseudoAxisEngineInfo *info,
				 const HklPseudoAxisEngineOperations *ops)
{
	self->info = info;
	self->ops = ops;
	list_head_init(&self->modes);
	hkl_parameter_list_init(&self->pseudo_axes,
				&hkl_parameter_list_operations_defaults);
	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;
}

void unregister_pseudo_axis(HklParameter *pseudo_axis)
{
	hkl_parameter_free(pseudo_axis);
}

HklParameter *register_pseudo_axis(HklPseudoAxisEngine *self,
				   const HklParameter *conf)
{
	HklParameter *parameter;

	parameter = hkl_parameter_new_pseudo_axis(conf, self);
	hkl_parameter_list_add_parameter(&self->pseudo_axes, parameter);

	return parameter;
}

static void hkl_pseudo_axis_engine_prepare_internal(HklPseudoAxisEngine *self)
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
		list_head_init(&self->axes);
		for(i=0; i<self->mode->info->n_axes; ++i){
			HklAxis *axis;

			axis = hkl_geometry_get_axis_by_name(self->geometry,
							     self->mode->info->axes[i]);

			list_add_tail(&self->axes, &axis->engine_list);
		}
	}

	/* reset the geometries len */
	hkl_geometry_list_reset(self->engines->geometries);
}

/**
 * hkl_pseudo_axis_engine_select_mode:
 * @self: the HklPseudoAxisEngine
 * @mode: the #HklPseudoAxisMode to select
 *
 * This method also populate the self->axes from the mode->axes_names.
 * this is to speed the computation of the numerical axes.
 **/
void hkl_pseudo_axis_engine_select_mode(HklPseudoAxisEngine *self,
					HklMode *mode)
{
	if(!self || !mode)
		return;

	self->mode = mode;
	hkl_pseudo_axis_engine_prepare_internal(self);
}

void hkl_pseudo_axis_engine_select_mode_by_name(HklPseudoAxisEngine *self,
						const char *name)
{
	HklMode *mode;

	if(!self || !name)
		return;

	list_for_each(&self->modes, mode, list){
		if(!strcmp(mode->info->name, name))
			hkl_pseudo_axis_engine_select_mode(self, mode);
	}
}

/**
 * hkl_pseudo_axis_engine_initialize: (skip)
 * @self: the HklPseudoAxisEngine
 * @error: (allow-none): NULL or an HklError to check for error's during the initialization
 *
 * initialize the HklPseudoAxisEngine
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_initialize(HklPseudoAxisEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self || !self->geometry || !self->detector || !self->sample
	   || !self->mode) {
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	/* a NULL initialize method is valid */
	if(self->mode->op->init
	   && !self->mode->op->init(self->mode,
				    self,
				    self->engines->geometry,
				    self->engines->detector,
				    self->engines->sample,
				    error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}

	hkl_assert(error == NULL || *error == NULL);

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_set: (skip)
 * @self: the HklPseudoAxisEngine
 * @error: (allow-none): NULL or an HklError
 *
 * use the HklPseudoaxisEngine values to compute the real axes values.
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_set(HklPseudoAxisEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self || !self->geometry || !self->detector || !self->sample
	   || !self->mode || !self->mode->op->set){
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	hkl_pseudo_axis_engine_prepare_internal(self);

	if (!self->mode->op->set(self->mode, self,
				 self->geometry,
				 self->detector,
				 self->sample,
				 error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	hkl_geometry_list_multiply(self->engines->geometries);
	hkl_geometry_list_multiply_from_range(self->engines->geometries);
	hkl_geometry_list_remove_invalid(self->engines->geometries);
	hkl_geometry_list_sort(self->engines->geometries, self->engines->geometry);

	if(self->engines->geometries->len == 0){
		hkl_error_set(error, "no remaining solutions");
		return HKL_FALSE;
	}

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_get: (skip)
 * @self: The HklPseudoAxisEngine
 * @error: (allow-none): NULL or an HklError
 *
 * get the values of the pseudo-axes from the real-axes values
 *
 * Returns:
 **/
int hkl_pseudo_axis_engine_get(HklPseudoAxisEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self || !self->engines || !self->engines->geometry || !self->engines->detector
	   || !self->engines->sample || !self->mode || !self->mode->op->get){
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	if (!self->mode->op->get(self->mode,
				 self,
				 self->engines->geometry,
				 self->engines->detector,
				 self->engines->sample,
				 error)){
		hkl_assert(error == NULL || *error != NULL);
		return HKL_FALSE;
	}
	hkl_assert(error == NULL || *error == NULL);

	return HKL_TRUE;
}

/**
 * hkl_pseudo_axis_engine_fprintf: (skip)
 * @f: the FILE
 * @self: the HklPseudoAxisEngine
 *
 * print to a FILE the HklPseudoAxisEngine
 **/
void hkl_pseudo_axis_engine_fprintf(FILE *f, const HklPseudoAxisEngine *self)
{
	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->info->name);

	/* mode */
	if (self->mode) {
		fprintf(f, " %s", self->mode->info->name);
		for(uint i=0; i<self->mode->parameters.len; ++i){
			fprintf(f, "\n     ");
			hkl_parameter_fprintf(
				f,
				self->mode->parameters.parameters[i]);
		}
		fprintf(f, "\n");
	}

	/* the pseudoAxes part */
	hkl_parameter_list_fprintf(f, &self->pseudo_axes);

	if(self->engines->geometries->len > 0){
		fprintf(f, "\n   ");
		hkl_geometry_list_fprintf(f, self->engines->geometries);
	}
	fprintf(f, "\n");
}

/***************************/
/* HklEngineList */
/***************************/

/**
 * hkl_engine_list_new: (skip)
 *
 * default constructor
 *
 * Returns:
 **/
HklEngineList *hkl_engine_list_new(void)
{
	HklEngineList *self = NULL;

	self = HKL_MALLOC(HklEngineList);

	list_head_init(&self->engines);

	self->geometries = hkl_geometry_list_new();

	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;

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
const HklEngineList *hkl_engine_list_new_copy(const HklEngineList *self)
{
	return NULL;
}

/**
 * hkl_engine_list_free: (skip)
 * @self: the #HklEngineList to destroy
 *
 * destructor
 **/
void hkl_engine_list_free(HklEngineList *self)
{
	hkl_engine_list_clear(self);
	hkl_geometry_list_free(self->geometries);
	free(self);
}

/**
 * hkl_engine_list_add: (skip)
 * @self: the engine list
 * @engine: the engine to add
 *
 * add an #HklPseudoAxisEngine to the #HklEngineList
 *
 * Returns: HKL_SUCCESS or HKL_FAIL
 **/
int hkl_engine_list_add(HklEngineList *self,
				    HklPseudoAxisEngine *engine)
{
	if (!engine)
		return HKL_FALSE;

	/* set the engines to access the Geometries list. */
	engine->engines = self;

	list_add_tail(&self->engines, &engine->list);

	return HKL_TRUE;
}

/**
 * hkl_engine_list_get_by_name:
 * @self: the this ptr
 * @name: the name of the requested #HklPseudoAxisEngin
 *
 * get the #HklPseudoAxisEngine by its name from the list.
 *
 * Returns: (transfer none) (allow-none): the requested engine
 **/
HklPseudoAxisEngine *hkl_engine_list_get_by_name(HklEngineList *self,
							     const char *name)
{
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list){
		if (!strcmp(engine->info->name, name))
			return engine;
	}

	return NULL;
}

/**
 * hkl_engine_list_get_pseudo_axis_by_name:
 * @self: the engine list
 * @name: the name of the requested #HklPseudoAxis
 *
 * Todo: test
 *
 * Returns: (transfer none) (allow-none): the requested #HklPseudoAxis
 **/
HklParameter *hkl_engine_list_get_pseudo_axis_by_name(
	const HklEngineList *self, const char *name)
{
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list){
		for(uint i=0; i<engine->pseudo_axes.len; ++i)
			if (!strcmp(engine->pseudo_axes.parameters[i]->name, name))
				return engine->pseudo_axes.parameters[i];
	}

	return NULL;
}

/**
 * hkl_engine_list_clear: (skip)
 * @self: the engine list to clear
 *
 * remove all engine from the engine list
 **/
void hkl_engine_list_clear(HklEngineList *self)
{
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngine *next;

	list_for_each_safe(&self->engines, engine, next, list){
		list_del(&engine->list);
		hkl_pseudo_axis_engine_free(engine);
	}
}

/**
 * hkl_engine_list_init:
 * @self: the engine list
 * @geometry: the associated #HklGeometry
 * @detector: the associated #HklDetector
 * @sample: the associated #HklSample
 *
 * before using an engine list you must associate all engines to a
 * Geometry, a detector and a sample.
 **/
void hkl_engine_list_init(HklEngineList *self,
				      HklGeometry *geometry,
				      HklDetector *detector,
				      HklSample *sample)
{
	HklPseudoAxisEngine *engine;

	self->geometry = geometry;
	self->detector = detector;
	self->sample = sample;

	list_for_each(&self->engines, engine, list){
		hkl_pseudo_axis_engine_prepare_internal(engine);
	}
}

/**
 * hkl_engine_list_get:
 * @self: the list of #HklPseudoAxisEngine
 *
 * apply the get method to all the #HklPseudoAxisEngine of the list
 * after this it is possible to retrive all the #HklPseudoAxis values.
 *
 * Returns: HKL_SUCCESS or HKL_FAIL if one of the #HklPseudoAxisEngine
 * get method failed.
 **/
int hkl_engine_list_get(HklEngineList *self)
{
	HklPseudoAxisEngine *engine;
	int res = HKL_TRUE;

	if (!self)
		return res;

	list_for_each(&self->engines, engine, list){
		res &= hkl_pseudo_axis_engine_get(engine, NULL);
	}

	return res;
}

/**
 * hkl_engine_list_fprintf: (skip)
 * @f: the File
 * @self: the list
 *
 * print to a FILE the #HklEngineList
 **/
void hkl_engine_list_fprintf(FILE *f,
					 const HklEngineList *self)
{
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list){
		hkl_pseudo_axis_engine_fprintf(f, engine);
	}
}
