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

#include "hkl-binding-private.h"

/**
 * hkl_geometry_get_axes_values_unit:
 * @self:
 * @len: (out caller-allocates)
 *
 * return all the axes values (must be free by the user)
 *
 * Returns: (array length=len) (transfer full):
 **/
double *hkl_geometry_get_axes_values_unit(const HklGeometry *self, unsigned int *len)
{
	double *values;
	uint i;

	if(!self || !len || self->len == 0)
		return NULL;

	*len = self->len;
	values = malloc(self->len * sizeof(*values));
	if(!values)
		return NULL;
	for(i=0; i<self->len; ++i)
		values[i] = hkl_axis_get_value_unit(&self->axes[i]);

	return values;
}

/**
 * hkl_geometry_set_axes_values_unit:
 * @self:
 * @values: (array length=len):
 * @len:
 *
 * set the axes values
 **/
void hkl_geometry_set_axes_values_unit(HklGeometry *self, double *values, unsigned int len)
{
	uint i;

	if (!self || !values || len != self->len)
		return;

	for(i=0; i<self->len; ++i)
		hkl_axis_set_value_unit(&self->axes[i], values[i]);
	hkl_geometry_update(self);
}

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

/**
 * hkl_pseudo_axis_engine_list_engines:
 * @self: the #HklPseudoAxisEngineList
 *
 * Return value: (element-type HklPseudoAxisEngine) (transfer container): list of engines,
 *               free the list with g_slist_free when done.
 **/
GSList* hkl_pseudo_axis_engine_list_engines(HklPseudoAxisEngineList *self)
{
	GSList *list = NULL;
	HklPseudoAxisEngine *engine;

	list_for_each(&self->engines, engine, list){
		list = g_slist_append(list, engine);
	}

	return list;
}
