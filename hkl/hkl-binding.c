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
 * Copyright (C) 2012-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <stdlib.h>                     // for malloc
#include <string.h>                     // for NULL, strdup
#include <sys/types.h>                  // for uint
#include "hkl-factory-private.h"        // for __start_xautodata_factories, etc
#include "hkl-geometry-private.h"       // for _HklGeometry, etc
#include "hkl-parameter-private.h"
#include "hkl-pseudoaxis-private.h"     // for _HklEngine, HklEngineInfo
#include "hkl-sample-private.h"         // for _HklSampleReflection, etc
#include "hkl.h"                        // for HklGeometry, HklEngine, etc
#include "hkl/ccan/autodata/autodata.h"  // for autodata_get
#include "hkl/ccan/darray/darray.h"     // for darray_foreach, darray_size
#include "hkl/ccan/list/list.h"             // for list_for_each, list_head

/**************/
/* HklFactory */
/**************/

/**
 * hkl_factories:
 *
 * return all the Hkl factories objects as a dictionnary
 *
 * Returns: (element-type utf8 Hkl.Factory) (transfer container):
 **/
GHashTable *hkl_factories(void)
{
	GHashTable *table = NULL;
	size_t i, n;
	HklFactory **factories;

	table = g_hash_table_new(g_str_hash, g_str_equal);
	factories = autodata_get(factories, &n);
	for(i=0; i<n; ++i){
		g_hash_table_insert(table,
				    (gpointer)hkl_factory_name_get(factories[i]),
				    factories[i]);
	}

	return table;
}

/************/
/* Geometry */
/************/

/**
 * hkl_geometry_axis_names_get_binding:
 * @self: the this ptr
 * @length: (out caller-allocates): the length of the returned array
 *
 * get all the axes of the given geometry.
 *
 * Rename to: hkl_geometry_axis_names_get
 *
 * Returns: (array length=length) (transfer none): array of the axes names.
 **/
const char **hkl_geometry_axis_names_get_binding(const HklGeometry *self,
						 size_t *length)
{
	const darray_string *axes = hkl_geometry_axis_names_get(self);

	*length = darray_size(*axes);

	return &darray_item(*axes, 0);
}

/**
 * hkl_geometry_axis_values_get_binding:
 * @self: the this ptr
 * @len: (out caller-allocates): the length of the returned array
 * @unit_type: the unit type (default or user) of the returned value
 *
 * Rename to: hkl_geometry_axis_values_get
 *
 * Return value: (array length=len) (transfer container): list of axes values,
 *          free the list with free when done.
 **/
double *hkl_geometry_axis_values_get_binding(const HklGeometry *self, guint *len,
					     HklUnitEnum unit_type)
{
	double *values;
	uint i = 0;
	HklParameter **axis;

	if(!self || !len || darray_size(self->axes) == 0)
		return NULL;

	*len = darray_size(self->axes);
	values = malloc(darray_size(self->axes) * sizeof(*values));

	darray_foreach(axis, self->axes){
		values[i++] = hkl_parameter_value_get(*axis, unit_type);
	}

	return values;
}

/*******************/
/* HklGeometryList */
/*******************/

/**
 * hkl_geometry_list_items:
 * @self: the #HklGeometryList
 *
 * Return value: (element-type HklGeometryListItem) (transfer container): list of items,
 *               free the list with g_slist_free when done.
 **/
GSList* hkl_geometry_list_items(HklGeometryList *self)
{
	GSList *list = NULL;
	HklGeometryListItem *item;

	list_for_each(&self->items, item, list)
		list = g_slist_append(list, item);

	return list;
}

/*************/
/* HklEngine */
/*************/

/**
 * hkl_engine_modes_names_get_binding:
 * @self: the this ptr
 * @length: (out caller-allocates): return the length of the returned array.
 *
 * Rename to: hkl_engine_modes_names_get
 *
 * Return value: (array length=length) (transfer none): All the modes supported by the #HklEngine
 **/
const char **hkl_engine_modes_names_get_binding(const HklEngine *self, size_t *length)
{
	*length = darray_size(self->mode_names);
	return &darray_item(self->mode_names, 0);
}

/**
 * hkl_engine_pseudo_axis_names_get_binding:
 * @self: the this ptr
 * @length: (out caller-allocates): return the length of the returned array.
 *
 * Rename to: hkl_engine_pseudo_axis_names_get
 *
 * Return value: (array length=length) (transfer none): All the pseudo_axes names of the #HklEngine
 *
 **/
const char **hkl_engine_pseudo_axis_names_get_binding(HklEngine *self, size_t *length)
{
	*length = darray_size(self->pseudo_axis_names);
	return &darray_item(self->pseudo_axis_names, 0);
}

/**
 * hkl_engine_parameters_names_get_binding:
 * @self: the this ptr
 * @length: (out caller-allocates): return the length of the returned array.
 *
 * Rename to: hkl_engine_parameters_names_get
 *
 * Return value: (array length=length) (transfer none): All the parameters of #HklEngine.
 **/
const char **hkl_engine_parameters_names_get_binding(const HklEngine *self, size_t *length)
{
	*length = darray_size(self->mode->parameters_names);
	return &darray_item(self->mode->parameters_names, 0);
}

/**
 * hkl_engine_axis_names_get_binding:
 * @self: the this ptr
 * @mode: the #HklEngineAxesNamesGet
 * @length: (out caller-allocates): return the length of the returned array.
 *
 * Rename to: hkl_engine_axis_names_get
 *
 * Return value: (array length=length) (transfer none): axes of the #HklEngine for the given mode.
 **/
const char **hkl_engine_axis_names_get_binding(const HklEngine *self,
					       HklEngineAxesNamesGet mode,
					       size_t *length)
{
	const darray_string *axes = hkl_engine_axis_names_get(self, mode);

	*length = darray_size(*axes);

	return &darray_item(*axes, 0);
}

/**
 * hkl_engine_pseudo_axis_values_get_binding:
 * @self: the this ptr
 * @len: (out caller-allocates): the length of the returned array
 * @unit_type: the unit type (default or user) of the returned value
 *
 * Rename to: hkl_engine_pseudo_axis_values_get
 *
 * Return value: (array length=len) (transfer container): list of pseudo axes values,
 *          free the list with free when done.
 **/
double *hkl_engine_pseudo_axis_values_get_binding(const HklEngine *self, guint *len,
						  HklUnitEnum unit_type)
{
	double *values;
	uint i = 0;
	HklParameter **axis;

	if(!self || !len || darray_size(self->pseudo_axes) == 0)
		return NULL;

	*len = darray_size(self->pseudo_axes);
	values = malloc(darray_size(self->pseudo_axes) * sizeof(*values));

	darray_foreach(axis, self->pseudo_axes){
		values[i++] = hkl_parameter_value_get(*axis, unit_type);
	}

	return values;
}

/**
 * hkl_engine_list_engines_get_as_gslist:
 * @self: the this ptr
 *
 * Return value: (element-type HklEngine) (transfer container): list of engines,
 *               free the list with g_slist_free when done.
 *
 * Rename to: hkl_engine_list_engines_get
 **/
GSList* hkl_engine_list_engines_get_as_gslist(HklEngineList *self)
{
	GSList *list = NULL;
	HklEngine **engine;

	darray_foreach(engine, *self){
		list = g_slist_append(list, *engine);
	}

	return list;
}

/*************/
/* HklSample */
/*************/

/**
 * hkl_sample_reflections_get:
 * @self: the this ptr
 *
 * Return value: (element-type HklSampleReflection) (transfer container): list of reflections,
 *               free the list with g_slist_free when done.
 */
GSList *hkl_sample_reflections_get(const HklSample *self)
{
	GSList *list = NULL;
	HklSampleReflection *reflection;

	list_for_each(&self->reflections, reflection, list){
		list = g_slist_append(list, reflection);
	}

	return list;
}

/**
 * hkl_sample_add_reflection_binding:
 * @self: the this ptr
 * @geometry: the geometry of the HklSampleReflection
 * @detector: the detector of the HklSampleReflection
 * @h: the h coordinate
 * @k: the k coordinate
 * @l: the l coordinate
 * @error: return location for a GError, or NULL
 *
 * Return value: (transfer none): the newly created HklSampleReflection
 *
 * Rename to: hkl_sample_add_reflection
 **/
HklSampleReflection *hkl_sample_add_reflection_binding(HklSample *self,
						       const HklGeometry *geometry,
						       const HklDetector *detector,
						       double h, double k, double l,
						       GError **error)
{
	HklSampleReflection *reflection;

	hkl_error (error == NULL || *error == NULL);

	reflection = hkl_sample_reflection_new(geometry, detector,
					       h, k, l, error);
	if(!reflection){
		g_assert (error == NULL || *error != NULL);
		return NULL;
	}
	g_assert (error == NULL || *error == NULL);

	hkl_sample_add_reflection(self, reflection);

	return reflection;
}
