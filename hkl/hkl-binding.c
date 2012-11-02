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
 * Copyright (C) 2012      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <glib.h>
#include <string.h>
#include "hkl-pseudoaxis-private.h"
#include "hkl-binding-private.h"

/********************/
/* HklParameterList */
/********************/

#define HKL_PARAMETER_LIST_ERROR hkl_parameter_list_error_quark ()

GQuark hkl_parameter_list_error_quark (void)
{
	return g_quark_from_static_string ("hkl-parameter-list-error-quark");
}

typedef enum {
	HKL_PARAMETER_LIST_ERROR_SET_VALUES /* can not set the parameter list values */
} HklParameterListError;

/**
 * hkl_parameter_list_set_values_unit_binding:
 * @self: the this ptr
 * @values: (array length=len): the values to set
 * @len: the length of the values
 * @error: error set if something goes wrong
 *
 * set the parameter list with the given values
 *
 * Rename to: hkl_parameter_list_set_values_unit
 *
 * Return value: true if succeed or false otherwise
 **/
gboolean hkl_parameter_list_set_values_unit_binding(HklParameterList *self,
						    double values[], uint len,
						    GError **error)
{
	HklError *err = NULL;

	g_return_val_if_fail(error == NULL ||*error == NULL, FALSE);

	if(!hkl_parameter_list_set_values_unit(self,
					       values, len, &err)){
		g_assert(&err == NULL || err != NULL);

		g_set_error(error,
			    HKL_PARAMETER_LIST_ERROR,
			    HKL_PARAMETER_LIST_ERROR_SET_VALUES,
			    strdup(err->message));

		hkl_error_clear(&err);

		return FALSE;
	}
	return TRUE;
}

/**
 * hkl_parameter_list_parameters:
 * @self: the this ptr
 *
 * Return value: (element-type HklParameter) (transfer container): list of parameters
 *               free the list with g_slist_free when done.
 **/
GSList* hkl_parameter_list_parameters(HklParameterList *self)
{
	GSList *list = NULL;
	HklParameter **parameter;

	darray_foreach(parameter, *self){
		list = g_slist_append(list, *parameter);
	}

	return list;
}

/************/
/* Geometry */
/************/

/**
 * hkl_geometry_axes:
 * @self: the this ptr
 *
 * Returns: (element-type HklAxis) (transfer container): list of HklAxis,
 *          free the list with g_slist_free when done.
 **/
GSList *hkl_geometry_axes(HklGeometry *self)
{
	GSList *list = NULL;
	guint i;

	for(i=0; i<self->len; ++i)
		list = g_slist_append(list, &self->axes[i]);

	return list;
}


/**
 * hkl_geometry_get_axes_values_unit:
 * @self: the this ptr
 * @len: (out caller-allocates): the length of the returned array
 *
 * Return value: (array length=len) (transfer container): list of axes values,
 *          free the list with free when done.
 **/
double *hkl_geometry_get_axes_values_unit(const HklGeometry *self, guint *len)
{
	double *values;
	uint i;

	if(!self || !len || self->len == 0)
		return NULL;

	*len = self->len;
	values = malloc(self->len * sizeof(*values));

	for(i=0; i<self->len; ++i)
		values[i] = hkl_parameter_get_value_unit(&self->axes[i].parameter);

	return values;
}

/**
 * hkl_geometry_set_axes_values_unit:
 * @self: the this ptr
 * @values: (array length=len): the values to set.
 * @len: the length of the values array.
 **/
void hkl_geometry_set_axes_values_unit(HklGeometry *self, double *values, unsigned int len)
{
	uint i;

	if (!self || !values || len != self->len)
		return;

	for(i=0; i<self->len; ++i)
		hkl_parameter_set_value_unit(&self->axes[i].parameter,
					     values[i],
					     NULL);
	hkl_geometry_update(self);
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

	list_for_each(&self->items, item, node){
		list = g_slist_append(list, item);
	}

	return list;
}

/***********************/
/* HklEngine */
/***********************/

#define HKL_ENGINE_ERROR hkl_engine_error_quark ()

GQuark hkl_engine_error_quark (void)
{
	return g_quark_from_static_string ("hkl-pseudo-axis-engine-error-quark");
}

typedef enum {
	HKL_ENGINE_ERROR_SET /* can not set the pseudo axis engine */
} HklEngineError;


/**
 * hkl_engine_modes:
 * @self: the this ptr
 *
 * Return value: (element-type HklMode) (transfer container): list of mdoe,
 *               free the list with g_slist_free when done.
 **/
GSList* hkl_engine_modes(HklEngine *self)
{
	GSList *list = NULL;
	HklMode **mode;

	darray_foreach(mode, self->modes){
		list = g_slist_append(list, *mode);
	}

	return list;
}

/**
 * hkl_engine_set_values_unit:
 * @self: the this ptr
 * @values: (array length=len): the values to set
 * @len: the len of the values array
 * @error: return location of a GError or NULL
 *
 * compute the #HklGeometry angles for this #HklEngine
 *
 * Return value: TRUE on success or FALSE if an error occurred
 **/
gboolean hkl_engine_set_values_unit(HklEngine *self,
				    double values[], unsigned int len,
				    GError **error)
{
	HklParameter *parameter;
	uint i = 0;
	HklError *err = NULL;

	g_return_val_if_fail(error == NULL ||*error == NULL, FALSE);

	if(len != self->info->n_pseudo_axes)
		return FALSE;

	if(!hkl_parameter_list_set_values_unit(&self->pseudo_axes,
					       values, len, &err)){
		g_assert(&err == NULL || err != NULL);

		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_SET,
			    strdup(err->message));

		hkl_error_clear(&err);

		return FALSE;
	}

	if(!hkl_engine_set(self, &err)){
		g_assert(&err == NULL || err != NULL);

		g_set_error(error,
			    HKL_ENGINE_ERROR,
			    HKL_ENGINE_ERROR_SET,
			    strdup(err->message));

		hkl_error_clear(&err);

		return FALSE;
	}
	g_assert(error != NULL ||*error != NULL);

	return TRUE;
}

/**
 * hkl_engine_list_engines_as_gslist:
 * @self: the this ptr
 *
 * Return value: (element-type HklEngine) (transfer container): list of engines,
 *               free the list with g_slist_free when done.
 *
 * Rename to: hkl_engine_list_engines
 **/
GSList* hkl_engine_list_engines_as_gslist(HklEngineList *self)
{
	GSList *list = NULL;
	HklEngine **engine;

	darray_foreach(engine, *self){
		list = g_slist_append(list, *engine);
	}

	return list;
}
