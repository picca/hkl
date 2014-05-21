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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>                      // for fprintf, FILE
#include <stdlib.h>                     // for free
#include <string.h>                     // for NULL, strcmp
#include <sys/types.h>                  // for uint
#include "hkl-detector-private.h"       // for hkl_detector_new_copy
#include "hkl-error-private.h"          // for hkl_error_set
#include "hkl-geometry-private.h"       // for _HklGeometryList, etc
#include "hkl-macros-private.h"         // for hkl_assert, HKL_MALLOC, etc
#include "hkl-parameter-private.h"      // for hkl_parameter_list_fprintf, etc
#include "hkl-pseudoaxis-private.h"     // for _HklEngine, _HklEngineList, etc
#include "hkl.h"                        // for HklEngine, HklEngineList, etc
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_foreach, darray_init, etc

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
	HklEngine *engine)
{
	HklPseudoAxis *self;

	self = HKL_MALLOC(HklPseudoAxis);

	self->parameter = *parameter;
	self->parameter.ops = &hkl_parameter_operations_pseudo_axis;
	self->engine = engine;

	return &self->parameter;
}

/***********/
/* HklMode */
/***********/

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
	fprintf(f, "initialize: %p\n", self->ops->init);
	fprintf(f, "get: %p\n", self->ops->get);
	fprintf(f, "set: %p\n", self->ops->set);
	hkl_parameter_list_fprintf(f, &self->parameters);
	if(self->info->axes){
		fprintf(f, "axes names:");
		for(i=0; i<self->info->n_axes; ++i)
			fprintf(f, " %s", self->info->axes[i]);
		fprintf(f, "\n");
	}
}

/*************/
/* HklEngine */
/*************/

void hkl_engine_init(HklEngine *self,
		     const HklEngineInfo *info,
		     const HklEngineOperations *ops)
{
	self->info = info;
	self->ops = ops;
	darray_init(self->modes);
	darray_init(self->pseudo_axes);
	darray_init(self->pseudo_axes_names);
	darray_init(self->mode_names);
	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;
}

void unregister_pseudo_axis(HklParameter *pseudo_axis)
{
	hkl_parameter_free(pseudo_axis);
}

HklParameter *register_pseudo_axis(HklEngine *self,
				   const HklParameter *conf)
{
	HklParameter *parameter;

	parameter = hkl_parameter_new_pseudo_axis(conf, self);
	darray_append(self->pseudo_axes, parameter);
	darray_append(self->pseudo_axes_names, parameter->name);

	return parameter;
}

static void hkl_engine_prepare_internal(HklEngine *self)
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
		darray_free(self->axes);
		darray_init(self->axes);
		for(i=0; i<self->mode->info->n_axes; ++i){
			HklParameter *axis;

			axis = hkl_geometry_get_axis_by_name(self->geometry,
							     self->mode->info->axes[i]);

			darray_append(self->axes, axis);
		}
	}

	/* reset the geometries len */
	hkl_geometry_list_reset(self->engines->geometries);
}

/**
 * hkl_engine_name_get:
 * @self: the this ptr
 *
 * Return value: the name of the HklEngine
 **/
const char *hkl_engine_name_get(const HklEngine *self)
{
	return self->info->name;
}

/**
 * hkl_engine_len: (skip)
 * @self: the this ptr
 *
 * Return value: the len of the pseudo axes of the HklEngine
 **/
unsigned int hkl_engine_len(const HklEngine *self)
{
	return self->info->n_pseudo_axes;
}

/**
 * hkl_engine_pseudo_axes_names_get:
 * @self: the this ptr
 *
 * Return value: (transfer none): the pseudo_axes managed by this HklEngine
 **/
const darray_string *hkl_engine_pseudo_axes_names_get(HklEngine *self)
{
	return &self->pseudo_axes_names;
}

/**
 * hkl_engine_pseudo_axes_values_get:
 * @self: the this ptr
 * @values: (array length=n_values): the values to get
 * @n_values: the size of the values array.
 *
 * Get the engine pseudo axes values
 **/
void hkl_engine_pseudo_axes_values_get(HklEngine *self,
				       double values[], size_t n_values)
{
	hkl_assert(n_values == self->info->n_pseudo_axes);

	for(size_t i=0; i<n_values; ++i)
		values[i] = hkl_parameter_value_get(darray_item(self->pseudo_axes, i));
}

/**
 * hkl_engine_pseudo_axes_values_set: (skip)
 * @self: the this ptr
 * @values: (array length=n_values): the values to set
 * @n_values: the size of the values array.
 * @error: (allow-none): the Error
 *
 * Set the engine pseudo axes values
 *
 * Returns: true if no-error was set or false otherwise.
 **/
unsigned int hkl_engine_pseudo_axes_values_set(HklEngine *self,
					       double values[], size_t n_values,
					       HklError **error)
{
	unsigned int res = HKL_TRUE;

	if(n_values != self->info->n_pseudo_axes){
		hkl_error_set(error, "cannot set engine pseudo axes, wrong number of parameter (%d) given, (%d) expected\n",
			      n_values, self->info->n_pseudo_axes);
		res = HKL_FALSE;
	} else {
		for(size_t i=0; i<n_values; ++i){
			if(!hkl_parameter_value_set(darray_item(self->pseudo_axes, i),
						    values[i], error)){
				res = HKL_FALSE;
				break;
			}
		}
	}
	return res;
}

/**
 * hkl_engine_pseudo_axes_randomize: (skip)
 * @self: the this ptr
 *
 * randomize all the parameters of the #HklEngine
 **/
void hkl_engine_pseudo_axes_randomize(HklEngine *self)
{
	HklParameter **parameter;

	darray_foreach(parameter, self->pseudo_axes){
		hkl_parameter_randomize(*parameter);
	}
}

/**
 * hkl_engine_pseudo_axis_get: (skip)
 * @self: the this ptr
 * @name: the name of the expected psudo_axis
 *
 * get the #HklParameter with the given @name.
 *
 * Returns: (allow-none): retun the parameter or NULL if the engine
 *                        does not contain this pseudo_axis.
 * TODO: unit test
 **/
const HklParameter *hkl_engine_pseudo_axis_get(const HklEngine *self,
					       const char *name)
{
	HklParameter **parameter;

	darray_foreach(parameter, self->pseudo_axes)
		if(!strcmp((*parameter)->name, name))
			return *parameter;
	return NULL;
}

/**
 * hkl_engine_pseudo_axis_set: (skip)
 * @self: the this ptr
 * @parameter: the parameter to set.
 * @error: the #HklError set.
 *
 * set a parameter of the #HklEngine
 * return value: HKL_TRUE if succeded or HKL_FALSE otherwise.
 * TODO: unit test
 **/
unsigned int hkl_engine_pseudo_axis_set(HklEngine *self,
					const HklParameter *parameter,
					HklError **error)
{
	HklParameter **p;

	darray_foreach(p, self->pseudo_axes)
		if(!strcmp((*p)->name, parameter->name)){
			hkl_parameter_init_copy(*p, parameter);
			return HKL_TRUE;
		}
	hkl_error_set(error,
		      "Can not find the pseudo axis \"%s\" in the \"%s\" engine\n",
		      parameter->name, self->info->name);
	return HKL_FALSE;
}

/**
 * hkl_engine_mode_set: (skip)
 * @self: the HklEngine
 * @name: the mode to select
 *
 * This method also populate the self->axes from the mode->axes_names.
 * this is to speed the computation of the numerical axes.
 **/
void hkl_engine_mode_set(HklEngine *self, HklMode *mode)
{
	self->mode = mode;
	hkl_engine_prepare_internal(self);
}

/**
 * hkl_engine_modes_get:
 * @self: the this ptr
 *
 * Return value: (type gpointer): All the modes supported by the #HklEngine
 **/
const darray_string *hkl_engine_modes_get(const HklEngine *self)
{
	return &self->mode_names;
}

/**
 * hkl_engine_parameters_get:
 * @self: the this ptr
 *
 * Return value: (type gpointer): All the parameters of #HklEngine.
 **/
const darray_string *hkl_engine_parameters_get(const HklEngine *self)
{
	return &self->mode->parameters_names;
}

/**
 * hkl_engine_parameters_set: (skip)
 * @self: the this ptr
 * @values: (array length=n_values): the values to set
 * @n_values: the size of the values array.
 * @error: (allow-none): the Error
 *
 * Set the engine parameters values
 *
 * Returns: true if no-error was set or false otherwise.
 **/
unsigned int hkl_engine_parameters_set(HklEngine *self,
				       double values[], size_t n_values,
				       HklError **error)
{
	unsigned int res = HKL_TRUE;

	if(n_values != darray_size(self->mode->parameters)){
		hkl_error_set(error, "cannot set engine parameters, wrong number of parameter (%d) given, (%d) expected\n",
			      n_values, darray_size(self->mode->parameters));
		res = HKL_FALSE;
	} else {
		for(size_t i=0; i<n_values; ++i){
			if(!hkl_parameter_value_set(darray_item(self->mode->parameters, i),
						    values[i], error)){
				res = HKL_FALSE;
				break;
			}
		}
	}
	return res;
}

/**
 * hkl_engine_parameters_randomize: (skip)
 * @self: the this ptr
 *
 * randomize all the parameters of the #HklEngine
 **/
void hkl_engine_parameters_randomize(HklEngine *self)
{
	HklParameter **parameter;

	darray_foreach(parameter, self->mode->parameters){
		hkl_parameter_randomize(*parameter);
	}
}

/**
 * hkl_engine_parameter_get: (skip)
 * @self: the this ptr
 * @name: the name of the expected parameter
 *
 * get the #HklParameter with the given @name.
 *
 * Returns: (allow-none): return the parameter or NULL if the engine
 *                        does not contain this parameter.
 **/
const HklParameter *hkl_engine_parameter_get(const HklEngine *self,
					     const char *name)
{
	HklParameter **parameter;

	darray_foreach(parameter, self->mode->parameters)
		if(!strcmp((*parameter)->name, name))
			return *parameter;
	return NULL;
}

/**
 * hkl_engine_parameter_set: (skip)
 * @self: the this ptr
 * @parameter: the parameter to set.
 *
 * set a parameter of the #HklEngine
 * TODO add an error
 **/
void hkl_engine_parameter_set(HklEngine *self, const HklParameter *parameter)
{
	HklParameter **p;

	darray_foreach(p, self->mode->parameters)
		if(!strcmp((*p)->name, parameter->name)){
			hkl_parameter_init_copy(*p, parameter);
			break;
		}
}

/**
 * hkl_engine_engines_get:
 * @self: the this ptr
 *
 * Return value: (transfer none): the HklEngineList which contain this HklEngine
 **/
HklEngineList *hkl_engine_engines_get(HklEngine *self)
{
	return self->engines;
}

/**
 * hkl_engine_select_mode:
 * @self: the HklEngine
 * @name: the mode to select
 *
 * This method also populate the self->axes from the mode->axes_names.
 * this is to speed the computation of the numerical axes.
 **/
void hkl_engine_select_mode(HklEngine *self, const char *name)
{
	HklMode **mode;

	darray_foreach(mode, self->modes)
		if(!strcmp((*mode)->info->name, name)){
			hkl_engine_mode_set(self, *mode);
			break;
		}
}

/**
 * hkl_engine_initialize: (skip)
 * @self: the HklEngine
 * @error: (allow-none): NULL or an HklError to check for error's during the initialization
 *
 * initialize the HklEngine
 *
 * Returns:
 **/
int hkl_engine_initialize(HklEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self->geometry || !self->detector || !self->sample
	   || !self->mode) {
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	/* a NULL initialize method is valid */
	if(self->mode->ops->init
	   && !self->mode->ops->init(self->mode,
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
 * hkl_engine_set: (skip)
 * @self: the HklEngine
 * @error: (allow-none): NULL or an HklError
 *
 * use the HklPseudoaxisEngine values to compute the real axes values.
 *
 * Returns:
 **/
int hkl_engine_set(HklEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self->geometry || !self->detector || !self->sample
	   || !self->mode || !self->mode->ops->set){
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	hkl_engine_prepare_internal(self);

	if (!self->mode->ops->set(self->mode, self,
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

	if(self->engines->geometries->n_items == 0){
		hkl_error_set(error, "no remaining solutions");
		return HKL_FALSE;
	}

	return HKL_TRUE;
}

/**
 * hkl_engine_get: (skip)
 * @self: The HklEngine
 * @error: (allow-none): NULL or an HklError
 *
 * get the values of the pseudo-axes from the real-axes values
 *
 * Returns:
 **/
int hkl_engine_get(HklEngine *self, HklError **error)
{
	hkl_return_val_if_fail (error == NULL || *error == NULL, HKL_FALSE);

	if(!self->engines || !self->engines->geometry || !self->engines->detector
	   || !self->engines->sample || !self->mode || !self->mode->ops->get){
		hkl_error_set(error, "Internal error");
		return HKL_FALSE;
	}

	if (!self->mode->ops->get(self->mode,
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
 * hkl_engine_fprintf: (skip)
 * @f: the FILE
 * @self: the HklEngine
 *
 * print to a FILE the HklEngine
 **/
void hkl_engine_fprintf(FILE *f, const HklEngine *self)
{
	fprintf(f, "\nPseudoAxesEngine : \"%s\"", self->info->name);

	/* mode */
	if (self->mode) {
		fprintf(f, " %s", self->mode->info->name);
		for(uint i=0; i<darray_size(self->mode->parameters); ++i){
			fprintf(f, "\n     ");
			hkl_parameter_fprintf(
				f,
				darray_item(self->mode->parameters, i));
		}
		fprintf(f, "\n");
	}

	/* the pseudoAxes part */
	hkl_parameter_list_fprintf(f, &self->pseudo_axes);

	if(self->engines->geometries->n_items != 0){
		fprintf(f, "\n   ");
		hkl_geometry_list_fprintf(f, self->engines->geometries);
	}
	fprintf(f, "\n");
}

/*****************/
/* HklEngineList */
/*****************/

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

	darray_init(*self);

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
 * add an #HklEngine to the #HklEngineList
 *
 * Returns: HKL_SUCCESS or HKL_FAIL
 **/
int hkl_engine_list_add(HklEngineList *self,
			HklEngine *engine)
{
	if (!engine)
		return HKL_FALSE;

	/* set the engines to access the Geometries list. */
	engine->engines = self;

	darray_append(*self, engine);

	return HKL_TRUE;
}

/**
 * hkl_engine_list_engines_get: (skip)
 * @self: the this ptr
 *
 * Return: a pointer on the engine array
 **/
darray_engine *hkl_engine_list_engines_get(HklEngineList *self)
{
	return (darray_engine *)self;
}

/**
 * hkl_engine_list_geometries_get:
 * @self: the this ptr
 *
 * Return: a pointer on the engine array
 **/
const HklGeometryList *hkl_engine_list_geometries_get(const HklEngineList *self)
{
	return self->geometries;
}

/**
 * hkl_engine_list_geometry_get: (skip)
 * @self: the this ptr
 *
 * Return: a pointer on the geometry member
 **/
HklGeometry *hkl_engine_list_geometry_get(HklEngineList *self)
{
	return self->geometry;
}

void hkl_engine_list_geometry_set(HklEngineList *self, const HklGeometry *geometry)
{
	hkl_geometry_set(self->geometry, geometry);
	hkl_engine_list_get(self);
}

/**
 * hkl_engine_list_select_solution:
 * @self: the this ptr
 * @item: the #HklGeoemtryListItem selected.
 *
 * this method set the geometry member with the selected solution.
 **/
void hkl_engine_list_select_solution(HklEngineList *self,
				     const HklGeometryListItem *item)
{
	hkl_geometry_init_geometry(self->geometry, item->geometry);
}

/**
 * hkl_engine_list_engine_get_by_name:
 * @self: the this ptr
 * @name: the name of the requested #HklPseudoAxisEngin
 *
 * get the #HklEngine by its name from the list.
 *
 * Returns: (transfer none) (allow-none): the requested engine
 **/
HklEngine *hkl_engine_list_engine_get_by_name(HklEngineList *self,
				       const char *name)
{
	HklEngine **engine;

	darray_foreach(engine, *self){
		if (!strcmp((*engine)->info->name, name))
			return *engine;
	}

	return NULL;
}

/**
 * hkl_engine_list_pseudo_axis_get_by_name:
 * @self: the engine list
 * @name: the name of the requested #HklPseudoAxis
 *
 * Todo: test
 *
 * Returns: (transfer none) (allow-none): the requested #HklPseudoAxis
 **/
HklParameter *hkl_engine_list_pseudo_axis_get_by_name(
	const HklEngineList *self, const char *name)
{
	HklEngine **engine;
	HklParameter **parameter;

	darray_foreach(engine, *self){
		darray_foreach(parameter, (*engine)->pseudo_axes){
			if (!strcmp((*parameter)->name, name))
				return *parameter;
		}
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
	HklEngine **engine;

	darray_foreach(engine, *self){
		hkl_engine_free(*engine);
	}
	darray_free(*self);
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
	HklEngine **engine;

	self->geometry = geometry;
	self->detector = detector;
	self->sample = sample;

	darray_foreach(engine, *self){
		hkl_engine_prepare_internal(*engine);
	}
}

/**
 * hkl_engine_list_get:
 * @self: the list of #HklEngine
 *
 * apply the get method to all the #HklEngine of the list
 * after this it is possible to retrive all the #HklPseudoAxis values.
 *
 * Returns: HKL_SUCCESS or HKL_FAIL if one of the #HklEngine
 * get method failed.
 **/
int hkl_engine_list_get(HklEngineList *self)
{
	HklEngine **engine;
	int res = HKL_TRUE;

	darray_foreach(engine, *self){
		res &= hkl_engine_get(*engine, NULL);
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
	HklEngine **engine;

	darray_foreach(engine, *self){
		hkl_engine_fprintf(f, *engine);
	}
}
