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
 * Copyright (C) 2003-2016 Synchrotron SOLEIL
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
#include "hkl-geometry-private.h"       // for _HklGeometryList, etc
#include "hkl-macros-private.h"         // for hkl_assert, HKL_MALLOC, etc
#include "hkl-parameter-private.h"      // for hkl_parameter_list_fprintf, etc
#include "hkl-pseudoaxis-private.h"     // for _HklEngine, _HklEngineList, etc
#include "hkl-sample-private.h"
#include "hkl.h"                        // for HklEngine, HklEngineList, etc
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_foreach, darray_init, etc

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
	HklParameter **parameter;
	const char **axis;

	fprintf(f, "mode: \"%s\"\n", self->info->name);
	fprintf(f, "initialized_get: %p\n", self->ops->initialized_get);
	fprintf(f, "initialized_set: %p\n", self->ops->initialized_set);
	fprintf(f, "get: %p\n", self->ops->get);
	fprintf(f, "set: %p\n", self->ops->set);

	darray_foreach(parameter, self->parameters){
		fprintf(f, "\n     ");
		hkl_parameter_fprintf(f, *parameter);
	}

	fprintf(f, "axes (read) names:");
	darray_foreach(axis, self->info->axes_r)
		fprintf(f, " %s", *axis);
	fprintf(f, "\n");

	fprintf(f, "axes (write) names:");
	darray_foreach(axis, self->info->axes_w)
		fprintf(f, " %s", *axis);
	fprintf(f, "\n");
}

/*************/
/* HklEngine */
/*************/

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
	return darray_size(self->info->pseudo_axes);
}

/**
 * hkl_engine_pseudo_axis_names_get:
 * @self: the this ptr
 *
 * Return value: (type gpointer): the pseudo_axes managed by this #HklEngine
 **/
const darray_string *hkl_engine_pseudo_axis_names_get(HklEngine *self)
{
	return &self->pseudo_axis_names;
}

/**
 * hkl_engine_pseudo_axis_values_get:
 * @self: the this ptr
 * @values: (array length=n_values): the values to get
 * @n_values: the size of the values array.
 * @unit_type: the unit type (default or user) of the returned value
 *
 * Get the engine pseudo axes values
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
int hkl_engine_pseudo_axis_values_get(HklEngine *self,
				      double values[], size_t n_values,
				      HklUnitEnum unit_type, GError **error)
{
	hkl_error(error == NULL ||*error == NULL);

	if(n_values != darray_size(self->info->pseudo_axes)){
		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_PSEUDO_AXIS_VALUES_GET,
			    "cannot get engine pseudo axes, wrong number of parameter (%d) given, (%d) expected\n",
			    n_values, darray_size(self->info->pseudo_axes));
		return FALSE;
	}

	if(!hkl_engine_get(self, error)){
		g_assert(error == NULL || *error != NULL);
		return FALSE;
	}
	g_assert(error == NULL || *error == NULL);

	for(size_t i=0; i<n_values; ++i){
		values[i] = hkl_parameter_value_get(darray_item(self->pseudo_axes, i),
						    unit_type);
	}
	return TRUE;
}

/**
 * hkl_engine_pseudo_axis_values_set:
 * @self: the this ptr
 * @values: (array length=n_values): the values to set
 * @n_values: the size of the values array.
 * @unit_type: the unit type (default or user) of the returned value
 * @error: return location for a GError, or NULL
 *
 * Set the engine pseudo axes values
 *
 * Return value: #HklGeometryList or NULL if no solution was found,
 *               use hkl_geometry_list_free to release the memory once done.
 **/
HklGeometryList *hkl_engine_pseudo_axis_values_set(HklEngine *self,
						   double values[], size_t n_values,
						   HklUnitEnum unit_type, GError **error)
{
#if LOGGING
	char *msg;
	size_t msg_size;
	FILE *stream;
#endif
	HklGeometryList *solutions = NULL;

	hkl_error(error == NULL ||*error == NULL);

	if(n_values != darray_size(self->info->pseudo_axes)){
		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_PSEUDO_AXIS_VALUES_SET,
			    "cannot set engine pseudo axes, wrong number of parameter (%d) given, (%d) expected\n",
			    n_values,  darray_size(self->info->pseudo_axes));
		goto out;
	}

#if LOGGING
	stream = open_memstream(&msg, &msg_size);

	fprintf(stream, "%s(", __func__);
	fprintf(stream, "self: %s, values: [", hkl_engine_name_get(self));
	for(size_t i=0; i<n_values; ++i)
		fprintf(stream, " %f", values[i]);
	fprintf(stream, "], n_values: %d unit_type: %d, error: %p)", n_values, unit_type, error);

	hkl_geometry_fprintf(stream, self->geometry);
	hkl_sample_fprintf(stream, self->sample);
	hkl_engine_fprintf(stream, self);
#endif
	for(size_t i=0; i<n_values; ++i){
		if(!hkl_parameter_value_set(darray_item(self->pseudo_axes, i),
					    values[i],
					    unit_type, error)){
			goto clean_stream_out;
		}
	}

	if(!hkl_engine_set(self, error)){
#if LOGGING
		fflush(stream);
		g_message(msg);
		if(error && *error != NULL)
			g_warning("%s", (*error)->message);
#endif
		goto clean_stream_out;
	}

	solutions = hkl_geometry_list_new_copy(self->engines->geometries);

clean_stream_out:
#if LOGGING
	fclose(stream);
	free(msg);
#endif
out:
	return solutions;
}

/**
 * hkl_engine_pseudo_axis_get:
 * @self: the this ptr
 * @name: the name of the expected pseudo_axis
 * @error: return location for a GError, or NULL
 *
 * get the #HklParameter with the given @name.
 *
 * Returns: (allow-none): retun the parameter or NULL if the engine
 *                        does not contain this pseudo_axis.
 * TODO: unit test
 **/
const HklParameter *hkl_engine_pseudo_axis_get(const HklEngine *self,
					       const char *name,
					       GError **error)
{
	HklParameter **parameter;

	hkl_error (error == NULL || *error == NULL);

	darray_foreach(parameter, self->pseudo_axes)
		if(!strcmp((*parameter)->name, name))
			return *parameter;

	g_set_error(error,
		    HKL_ENGINE_ERROR,
		    HKL_ENGINE_ERROR_PSEUDO_AXIS_SET,
		    "This pseudo axis doesn not contain this pseudo axis \"%s\"\n",
		    name);

	return NULL;
}

/**
 * hkl_engine_pseudo_axis_set: (skip)
 * @self: the this ptr
 * @name: the name of the pseudo_axis to set
 * @parameter: the parameter to set.
 * @error: return location for a GError, or NULL
 *
 * set a parameter of the #HklEngine
 *
 * Return value: #HklGeometryList or NULL if no solution was found,
 *               use hkl_geometry_list_free to release the memory once done.
 **/
HklGeometryList *hkl_engine_pseudo_axis_set(HklEngine *self,
					    const char *name,
					    const HklParameter *parameter,
					    GError **error)
{
	HklParameter **p;

	hkl_error (error == NULL || *error == NULL);
	hkl_error (strcmp(name, parameter->name) == 0);

	darray_foreach(p, self->pseudo_axes)
		if(!strcmp((*p)->name, parameter->name)){
			/* todo save the previous value to restore this value */
			hkl_parameter_init_copy(*p, parameter, NULL);
			if(!hkl_engine_set(self, error)){
				g_assert(error == NULL || *error != NULL);
				return NULL;
			}
			g_assert(error == NULL || *error == NULL);

			return hkl_geometry_list_new_copy(self->engines->geometries);
		}

	g_set_error(error,
		    HKL_ENGINE_ERROR,
		    HKL_ENGINE_ERROR_PSEUDO_AXIS_SET,
		    "Can not find the pseudo axis \"%s\" in the \"%s\" engine\n",
		    parameter->name, self->info->name);

	return NULL;
}

/**
 * hkl_engine_capabilities_get:
 * @self: the this ptr
 *
 * return the capabilities of the engine. Theses capabilities can
 * change, from on mode to the other. for now there is three kind of
 * capabilities.
 *
 * HKL_ENGINE_CAP_READABLE: pseudo axes values can be read from the HklEngine
 *
 * HKL_ENGINE_CAP_WRITABLE: pseudo axes values can be set for this #HklEngine
 *
 * HKL_ENGINE_CAP_INITIALISABLE: this pseudo axes must be initialized
 *                               with hkl_engine_initialize before
 *                               reading or writing on it.
 *
 * Returns:
 **/
unsigned int hkl_engine_capabilities_get(const HklEngine *self)
{
	return self->mode->ops->capabilities;
}


/**
 * hkl_engine_modes_names_get:
 * @self: the this ptr
 *
 * Return value: (type gpointer): All the modes supported by the #HklEngine
 **/
const darray_string *hkl_engine_modes_names_get(const HklEngine *self)
{
	return &self->mode_names;
}

/**
 * hkl_engine_parameters_names_get:
 * @self: the this ptr
 *
 * Return value: (type gpointer): All the parameters of #HklEngine.
 **/
const darray_string *hkl_engine_parameters_names_get(const HklEngine *self)
{
	return &self->mode->parameters_names;
}

/**
 * hkl_engine_parameters_values_get: (skip)
 * @self: the this ptr
 * @values: (array length=n_values): the values to get
 * @n_values: the size of the values array.
 * @unit_type: the unit type (default or user) of the returned value
 *
 * Get the engine parameters values
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
void hkl_engine_parameters_values_get(const HklEngine *self,
				      double values[], size_t n_values,
				      HklUnitEnum unit_type)
{
	g_return_if_fail (n_values == darray_size(self->mode->parameters));

	for(size_t i=0; i<n_values; ++i)
		values[i] = hkl_parameter_value_get(darray_item(self->mode->parameters, i),
						    unit_type);
}

/**
 * hkl_engine_parameters_values_set:
 * @self: the this ptr
 * @values: (array length=n_values): the values to set
 * @n_values: the size of the values array.
 * @unit_type: the unit type (default or user) of the returned value
 * @error: return location for a GError, or NULL
 *
 * Set the engine parameters values
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
int hkl_engine_parameters_values_set(HklEngine *self,
				     double values[], size_t n_values,
				     HklUnitEnum unit_type, GError **error)
{
	hkl_error (error == NULL || *error == NULL || n_values == darray_size(self->mode->parameters));

	for(size_t i=0; i<n_values; ++i){
		if(!hkl_parameter_value_set(darray_item(self->mode->parameters, i),
					    values[i], unit_type, error)){
			g_assert (error == NULL || *error != NULL);
			return FALSE;
		}
	}
	g_assert (error == NULL || *error == NULL);

	return TRUE;
}

/**
 * hkl_engine_parameter_get:
 * @self: the this ptr
 * @name: the name of the expected parameter
 * @error: return location for a GError, or NULL
 *
 * get the #HklParameter with the given @name.
 *
 * Returns: (allow-none): return the parameter or NULL if the engine
 *                        does not contain this parameter.
 **/
const HklParameter *hkl_engine_parameter_get(const HklEngine *self,
					     const char *name,
					     GError **error)
{
	HklParameter **parameter;

	hkl_error (error == NULL || *error == NULL);

	darray_foreach(parameter, self->mode->parameters)
		if(!strcmp((*parameter)->name, name))
			return *parameter;

	g_set_error(error,
		    HKL_ENGINE_ERROR,
		    HKL_ENGINE_ERROR_PARAMETER_GET,
		    "this engine does not contain this parameter \"%s\"\n",
		    name);

	return NULL;
}

/**
 * hkl_engine_parameter_set:
 * @self: the this ptr
 * @name: the name of the parameter to set.
 * @parameter: the parameter to set.
 * @error: return location for a GError, or NULL
 *
 * set a parameter of the #HklEngine
 * TODO add an error
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
int hkl_engine_parameter_set(HklEngine *self,
			     const char *name,
			     const HklParameter *parameter,
			     GError **error)
{
	HklParameter **p;

	hkl_error (error == NULL || *error == NULL);

	darray_foreach(p, self->mode->parameters)
		if(!strcmp(name, (*p)->name)){
			const char *old_name = (*p)->name;
			hkl_parameter_init_copy(*p, parameter, NULL);
			/* we do not check if the name is identical so force the right name */
			/* TODO rethink this HklParameter assignement */
			(*p)->name = old_name;
			return TRUE;
		}

	g_set_error(error,
		    HKL_ENGINE_ERROR,
		    HKL_ENGINE_ERROR_PARAMETER_SET,
		    "this engine does not contain this parameter \"%s\"\n",
		    parameter->name);

	return FALSE;
}

/**
 * hkl_engine_current_mode_get:
 * @self: the this ptr
 *
 * Returns: the current HklEngine mode
 **/
const char *hkl_engine_current_mode_get(const HklEngine *self)
{
	return self->mode->info->name;
}

/**
 * hkl_engine_current_mode_set:
 * @self: the HklEngine
 * @name: the mode to select
 * @error: return location for a GError, or NULL
 *
 * This method also populate the self->axes from the mode->axis_names.
 * this is to speed the computation of the numerical axes.
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
int hkl_engine_current_mode_set(HklEngine *self, const char *name,
				GError **error)
{
	HklMode **mode;

	hkl_error (error == NULL || *error == NULL);

	darray_foreach(mode, self->modes)
		if(!strcmp((*mode)->info->name, name)){
			hkl_engine_mode_set(self, *mode);
			return TRUE;
		}

	g_set_error(error,
		    HKL_ENGINE_ERROR,
		    HKL_ENGINE_ERROR_CURRENT_MODE_SET,
		    "this engine does not contain this mode \"%s\"\n",
		    name);

	return FALSE;
}

/**
 * hkl_engine_axis_names_get:
 * @self: the this ptr
 * @mode:
 *
 * return a list of axes relevant when reading or writing on the
 * #HklEngine.
 *
 * exemple, for a K6C diffractometer in "lifting detector" mode, your
 * Engine control only 3 motors when writing, but the hkl values
 * depends on the 6 motors when reading.
 *
 * Returns: (type gpointer):
 **/
const darray_string *hkl_engine_axis_names_get(const HklEngine *self,
					       HklEngineAxisNamesGet mode)
{
	switch(mode){
	case HKL_ENGINE_AXIS_NAMES_GET_READ:
		return &self->mode->info->axes_r;
	case HKL_ENGINE_AXIS_NAMES_GET_WRITE:
		return &self->mode->info->axes_w;
	default:
		return NULL;
	}
}

int hkl_engine_initialized_get(const HklEngine *self)
{
	return hkl_mode_initialized_get(self->mode);
}

/**
 * hkl_engine_initialized_set:
 * @self: the HklEngine
 * @initialized: TRUE or FALSE to activated or deactivate the HklEngine
 * @error: return location for a GError, or NULL
 *
 * initialize the HklEngine
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
int hkl_engine_initialized_set(HklEngine *self, int initialized, GError **error)
{
	hkl_error (error == NULL || *error == NULL);

	if(!self->geometry || !self->detector || !self->sample
	   || !self->mode) {
		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_INITIALIZE,
			    "Internal error");
		return FALSE;
	}

	return hkl_mode_initialized_set(self->mode,
					self,
					self->engines->geometry,
					self->engines->detector,
					self->engines->sample,
					initialized,
					error);
}

/**
 * hkl_engine_dependencies_get:
 * @self: the this ptr
 *
 * return all dependencies of the engine which are not mode parameters
 * or axes. The possible combination of values are defined in
 * #HklEngineDependencies enum.
 *
 * return value:
 **/
unsigned int hkl_engine_dependencies_get(const HklEngine *self)
{
	return self->info->dependencies;
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
	HklParameter **pseudo_axis;

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
	darray_foreach(pseudo_axis, self->pseudo_axes){
		fprintf(f, "\n     ");
		hkl_parameter_fprintf(f, *pseudo_axis);
	}

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
 * hkl_engine_list_geometry_get: (skip)
 * @self: the this ptr
 *
 * Return: a pointer on the geometry member
 **/
HklGeometry *hkl_engine_list_geometry_get(HklEngineList *self)
{
	return self->geometry;
}

int hkl_engine_list_geometry_set(HklEngineList *self, const HklGeometry *geometry)
{
	if(!hkl_geometry_set(self->geometry, geometry))
		return FALSE;

	hkl_engine_list_get(self);

	return TRUE;
}

/**
 * hkl_engine_list_select_solution:
 * @self: the this ptr
 * @item: the #HklGeoemtryListItem selected.
 *
 * this method set the geometry member with the selected solution.
 *
 * return value: TRUE if succeded or FALSE otherwise.
 **/
int hkl_engine_list_select_solution(HklEngineList *self,
				    const HklGeometryListItem *item)
{
	return hkl_geometry_init_geometry(self->geometry, item->geometry);
}

/**
 * hkl_engine_list_engine_get_by_name:
 * @self: the this ptr
 * @name: the name of the requested #HklPseudoAxisEngin
 * @error: return location for a GError, or NULL
 *
 * get the #HklEngine by its name from the list.
 *
 * Returns: (transfer none) (allow-none): the requested engine
 **/
HklEngine *hkl_engine_list_engine_get_by_name(HklEngineList *self,
					      const char *name,
					      GError **error)
{
	HklEngine **engine;

	hkl_error (error == NULL || *error == NULL);

	darray_foreach(engine, *self){
		if (!strcmp((*engine)->info->name, name))
			return *engine;
	}

	g_set_error(error,
		    HKL_ENGINE_LIST_ERROR,
		    HKL_ENGINE_LIST_ERROR_ENGINE_GET_BY_NAME,
		    "this engine list does not contain this engine \"%s\"",
		    name);

	return NULL;
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
	int res = TRUE;

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
