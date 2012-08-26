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
 * Copyright (C) 2003-2011 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-parameter-private.h>

static int hkl_parameter_init(HklParameter *self, const char *name,
			      double min, double value, double max,
			      int fit, int changed,
			      const HklUnit *unit, const HklUnit *punit)
{
	if (min <= value
	    && value <= max
	    && strcmp(name, "")
	    && hkl_unit_compatible(unit, punit)) {
		self->name = name;
		self->range.min = min;
		self->range.max = max;
		self->_value = value;
		self->unit = unit;
		self->punit = punit;
		self->fit = fit;
		self->changed = changed;
		self->ops = &hkl_parameter_operations_defaults;
	} else
		return HKL_FALSE;

	return HKL_TRUE;
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
HklParameter *hkl_parameter_new(const char *name,
				double min, double value, double max,
				int fit, int changed,
				const HklUnit *unit, const HklUnit *punit)
{
	HklParameter *self;

	self = HKL_MALLOC(HklParameter);

	if (!hkl_parameter_init(self,
				name, min, value, max,
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
	HklParameter *parameter = NULL;

	parameter = HKL_MALLOC(HklParameter);

	*parameter = *self;

	return parameter;
}

/**
 * hkl_parameter_free: (skip)
 * @self:
 *
 * delete an #HklParameter
 **/
void hkl_parameter_free(HklParameter *self)
{
	if(self)
		free(self);
}

/**
 * hkl_parameter_get_value:
 * @self: the this ptr
 *
 * Returns: the value of the #HklParameter
 **/
inline double hkl_parameter_get_value(const HklParameter *self)
{
	return self->ops->get_value(self);
}

/**
 * hkl_parameter_get_value_unit:
 * @self: the this ptr
 *
 * Returns: the value of the #HklParameter expressed in the user unit
 **/
inline double hkl_parameter_get_value_unit(const HklParameter *self)
{
	return self->ops->get_value_unit(self);
}

/**
 * hkl_parameter_set_value: (skip)
 * @self:
 * @value:
 *
 * set the value of an #HklParameter
 **/
inline void hkl_parameter_set_value(HklParameter *self, double value)
{
	self->ops->set_value(self, value);
}

/**
 * hkl_parameter_set_value_unit: (skip)
 * @self: the this ptr
 * @value: the value to set
 *
 * set the value of the parameter express in the punit #HklUnit
 * @todo test
 **/
inline void hkl_parameter_set_value_unit(HklParameter *self, double value)
{
	self->ops->set_value_unit(self, value);
}

/**
 * hkl_parameter_get_max: (skip)
 * @self:
 *
 * get the max value of the #HklParameter
 *
 * Returns:
 **/
double hkl_parameter_get_max(const HklParameter *self)
{
	return self->range.max;
}

/**
 * hkl_parameter_get_range_unit: (skip)
 * @self:
 * @min:
 * @max:
 *
 * get the #HklParameter range, min, max
 * @todo test
 **/
void hkl_parameter_get_range_unit(const HklParameter *self, double *min, double *max)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	*min = factor * self->range.min;
	*max = factor * self->range.max;
}

/**
 * hkl_parameter_set_range: (skip)
 * @self:
 * @min:
 * @max:
 *
 * set the #HklParameter range.
 * @todo test
 **/
void hkl_parameter_set_range(HklParameter *self, double min, double max)
{
	self->range.min = min;
	self->range.max = max;
}

/**
 * hkl_parameter_set_range_unit: (skip)
 * @self:
 * @min:
 * @max:
 *
 * set the #HklParameter range express in the punit #HklUnit
 * @todo test
 **/
void hkl_parameter_set_range_unit(HklParameter *self, double min, double max)
{
	double factor = hkl_unit_factor(self->unit, self->punit);
	self->range.min = min / factor;
	self->range.max = max / factor;
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
	if(self->_value < (self->range.min - HKL_EPSILON)
	   || self->_value > (self->range.max + HKL_EPSILON))
		return HKL_FALSE;
	else
		return HKL_TRUE;
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
