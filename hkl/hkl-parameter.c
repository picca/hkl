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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>                      // for fprintf, FILE
#include <stdlib.h>                     // for free, malloc, NULL
#include <string.h>                     // for strcmp
#include "hkl-interval-private.h"       // for HklInterval
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-parameter-private.h"      // for _HklParameter, etc
#include "hkl-unit-private.h"           // for hkl_unit_factor, HklUnit, etc
#include "hkl.h"                        // for HklParameter, etc
#include "hkl/ccan/darray/darray.h"     // for darray_size, darray_item, etc

/****************/
/* HklParameter */
/****************/

static int hkl_parameter_init(HklParameter *self, const char *name,
			      const char *description,
			      double min, double value, double max,
			      int fit, int changed,
			      const HklUnit *unit, const HklUnit *punit)
{
	if (min <= value
	    && value <= max
	    && strcmp(name, "")
	    && strcmp(description, "")
	    && hkl_unit_compatible(unit, punit)) {
		self->name = name;
		self->description = description;
		self->range.min = min;
		self->range.max = max;
		self->_value = value;
		self->unit = unit;
		self->punit = punit;
		self->fit = fit;
		self->changed = changed;
		self->ops = &hkl_parameter_operations_defaults;
	} else
		return FALSE;

	return TRUE;
}

/**
 * hkl_parameter_new: (skip)
 * @name:
 * @min:
 * @value:
 * @max:
 * @fit:
 * @changed:
 * @unit:
 * @punit:
 *
 * create a new #HklParameter
 *
 * Returns:
 **/
HklParameter *hkl_parameter_new(const char *name, const char *description,
				double min, double value, double max,
				int fit, int changed,
				const HklUnit *unit, const HklUnit *punit)
{
	HklParameter *self;

	self = HKL_MALLOC(HklParameter);

	if (!hkl_parameter_init(self,
				name, description,
				min, value, max,
				fit, changed,
				unit, punit)) {
		free(self);
		self = NULL;
	}

	return self;
}

/**
 * hkl_parameter_new_copy: (skip)
 * @self:
 *
 * copy an #HklParameter
 *
 * Returns:
 **/
HklParameter *hkl_parameter_new_copy(const HklParameter *self)
{
	return self->ops->copy(self);
}

/**
 * hkl_parameter_free: (skip)
 * @self:
 *
 * delete an #HklParameter
 **/
void hkl_parameter_free(HklParameter *self)
{
	self->ops->free(self);
}

/**
 * hkl_parameter_init_copy: (skip)
 * @self: the this ptr
 * @src: the parameter to copy from
 * @error: return location for a GError, or NULL
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
int hkl_parameter_init_copy(HklParameter *self, const HklParameter *src,
			    GError **error)
{
	return self->ops->init_copy(self, src, error);
}

/**
 * hkl_parameter_name_get:
 * @self: the this ptr
 *
 * Returns: the name of the #HklParameter
 **/
const char *hkl_parameter_name_get(const HklParameter *self)
{
	return self->name;
}

/**
 * hkl_parameter_default_unit_get:
 * @self: the this ptr
 *
 * Returns: the default unit of the #HklParameter
 **/
const char *hkl_parameter_default_unit_get(const HklParameter *self)
{
	return self->unit->name;
}

/**
 * hkl_parameter_user_unit_get:
 * @self: the this ptr
 *
 * Returns: the user unit of the #HklParameter
 **/
const char *hkl_parameter_user_unit_get(const HklParameter *self)
{
	return self->punit->name;
}

/**
 * hkl_parameter_value_get:
 * @self: the this ptr
 * @unit_type: the unit type (default or user) of the returned value
 *
 * Returns: the value of the #HklParameter
 **/
inline double hkl_parameter_value_get(const HklParameter *self,
				      HklUnitEnum unit_type)
{
	switch(unit_type){
	case HKL_UNIT_DEFAULT:
		return self->_value;
		break;
	case HKL_UNIT_USER:
		return self->_value * hkl_unit_factor(self->unit, self->punit);
		break;
	}
}

/**
 * hkl_parameter_value_get_closest:
 * @self: the this ptr
 * @ref: the reference #HklParameter
 *
 *
 *
 * Returns: the closest value of the ref #HklParameter from the
 *          current self #HklParameter
 **/
inline double hkl_parameter_value_get_closest(const HklParameter *self,
					      const HklParameter *ref)
{
	return self->ops->get_value_closest(self, ref);
}

/**
 * hkl_parameter_value_set:
 * @self: this ptr
 * @value: the value to set
 * @unit_type: the unit type (default or user) of the returned value
 * @error: return location for a GError, or NULL
 *
 * set the value of an #HklParameter
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
inline int hkl_parameter_value_set(HklParameter *self, double value,
				   HklUnitEnum unit_type, GError **error)
{
	return self->ops->set_value(self, value, unit_type, error);
}

/**
 * hkl_parameter_value_set_smallest_in_range: (skip)
 * @self: the this ptr
 **/
inline void hkl_parameter_value_set_smallest_in_range(HklParameter *self)
{
	self->ops->set_value_smallest_in_range(self);
}

/**
 * hkl_parameter_min_max_get:
 * @self: the this ptr
 * @min: (out caller-allocates): the returned minimum value
 * @max: (out caller-allocates): the returned maximum value
 * @unit_type: the unit type (default or user) of the returned values
 *
 * get the min and max value of the #HklParameter
 *
 **/
void hkl_parameter_min_max_get(const HklParameter *self, double *min, double *max,
			       HklUnitEnum unit_type)
{
	double factor;

	switch (unit_type){
	case HKL_UNIT_DEFAULT:
		*min = self->range.min;
		*max = self->range.max;
		break;
	case HKL_UNIT_USER:
		factor = hkl_unit_factor(self->unit, self->punit);
		*min = factor * self->range.min;
		*max = factor * self->range.max;
		break;
	}
}

/**
 * hkl_parameter_min_max_set:
 * @self: the this ptr
 * @min: the minimum value to set
 * @max: the maximum value to set
 * @unit_type: the unit type (default or user) of the min, max
 * @error: return location for a GError, or NULL
 *
 * set the #HklParameter range.
 * @todo test and set the GError
 *
 * Returns: TRUE on success, FALSE if an error occurred
 **/
int hkl_parameter_min_max_set(HklParameter *self, double min, double max,
			      HklUnitEnum unit_type, GError **error)
{
	double factor;

	hkl_error (error == NULL || *error == NULL);

	if (min > max){
		g_set_error(error,
			    HKL_PARAMETER_ERROR,
			    HKL_PARAMETER_ERROR_MIN_MAX_SET,
			    "can not set this range min > max\n");

		return FALSE;
	}

	switch (unit_type){
	case HKL_UNIT_DEFAULT:
		self->range.min = min;
		self->range.max = max;
		break;
	case HKL_UNIT_USER:
		factor = hkl_unit_factor(self->unit, self->punit);
		self->range.min = min / factor;
		self->range.max = max / factor;
		break;
	}

	return TRUE;
}

/**
 * hkl_parameter_fit_get:
 * @self: the this ptr
 *
 * Retuen value: the #HklParameter fit value, True is the parameter can be fitted, not otherwise
 * @todo test
 **/
int hkl_parameter_fit_get(const HklParameter *self)
{
	return self->fit;
}

/**
 * hkl_parameter_fit_set:
 * @self: the this ptr
 * @fit: the fit value to set
 *
 * set the #HklParameter fit value, True is the parameter can be fitted, not otherwise
 * @todo test
 **/
void hkl_parameter_fit_set(HklParameter *self, int fit)
{
	self->fit = fit;
}

/**
 * hkl_parameter_randomize: (skip)
 * @self:
 *
 * randomize the #HklParameter value into the min,max range
 **/
void hkl_parameter_randomize(HklParameter *self)
{
	self->ops->randomize(self);
}

/**
 * hkl_parameter_is_valid: (skip)
 * @self:
 *
 * check if the value of the #HklParameter is in the min,max range
 *
 * Returns:
 **/
int hkl_parameter_is_valid(const HklParameter *self)
{
	return self->ops->is_valid(self);
}

/**
 * hkl_parameter_fprintf: (skip)
 * @f:
 * @self:
 *
 * print into the #FILE f an #HklParameter
 **/
void hkl_parameter_fprintf(FILE *f, HklParameter *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);
	if (self->punit)
		fprintf(f, "\"%s\" : %.7f %s [%.7f : %.7f] (%d)",
			self->name,
			self->_value * factor,
			self->punit->repr,
			self->range.min * factor,
			self->range.max * factor,
			self->fit);
	else
		fprintf(f, "\"%s\" : %.7f [%.7f : %.7f] (%d)",
			self->name,
			self->_value * factor,
			self->range.min * factor,
			self->range.max * factor,
			self->fit);
}

/**
 * hkl_parameter_axis_v_get:
 * @self: the this ptr
 *
 * Returns: (allow-none):
 **/
const HklVector *hkl_parameter_axis_v_get(const HklParameter *self)
{
	return self->ops->axis_v_get(self);
}

/**
 * hkl_parameter_quaternion_get:
 * @self: the this ptr
 *
 * Returns: (allow-none):
 **/
const HklQuaternion *hkl_parameter_quaternion_get(const HklParameter *self)
{
	return self->ops->quaternion_get(self);
}

/**
 * hkl_parameter_description_get:
 * @self: the this ptr
 *
 * Returns: the #HklParameter description
 **/
const char *hkl_parameter_description_get(const HklParameter *self)
{
	return self->description;
}
