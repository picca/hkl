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

/****************/
/* HklParameter */
/****************/

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
	return self->ops->copy(self);
}

/**
 * hkl_parameter_free: (skip)
 * @self:
 *
 * delete an #HklParameter
extern HklAxis *hkl_axis_new_copy(const HklAxis *self);

 **/
void hkl_parameter_free(HklParameter *self)
{
	self->ops->free(self);
}

/**
 * hkl_parameter_get_value:
 * @self: the this ptr
 *
 * Returns: the value of the #HklParameter
 **/
inline double hkl_parameter_get_value(const HklParameter *self)
{
	return self->_value;
}

/**
 * hkl_parameter_get_value_unit:
 * @self: the this ptr
 *
 * Returns: the value of the #HklParameter expressed in the user unit
 **/
inline double hkl_parameter_get_value_unit(const HklParameter *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	return self->_value * factor;
}


/**
 * hkl_parameter_get_value_closest:
 * @self: the this ptr
 * @ref: the reference #HklParameter
 *
 * 
 *
 * Returns: the closest value of the ref #HklParameter from the
 *          current self #HklParameter
 **/
inline double hkl_parameter_get_value_closest(const HklParameter *self,
					      const HklParameter *ref)
{
	return self->ops->get_value_closest(self, ref);
}

/**
 * hkl_parameter_set_value: (skip)
 * @self: this ptr
 * @value: the value to set
 * @error: the error set if something goes wrong
 *
 * set the value of an #HklParameter
 *
 * Return value: true if succeed or false otherwise
 **/
inline bool hkl_parameter_set_value(HklParameter *self, double value,
				    HklError **error)
{
	return self->ops->set_value(self, value, error);
}

/**
 * hkl_parameter_set_value_unit: (skip)
 * @self: the this ptr
 * @value: the value to set
 * @error: the error set if something goes wrong
 *
 * set the value of the parameter express in the punit #HklUnit
 * @todo test
 *
 * Return value: true if succeed or false otherwise
 **/
inline bool hkl_parameter_set_value_unit(HklParameter *self, double value,
					 HklError **error)
{
	return self->ops->set_value_unit(self, value, error);
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

/********************/
/* HklParameterList */
/********************/

/**
 * hkl_parameter_list_add_parameter: (skip)
 * @self: the this ptr
 * @parameter: the #HklParameter to add to the list
 *
 * add an #HklParameter to a list of parameter.
 **/
void hkl_parameter_list_add_parameter(HklParameterList *self, HklParameter *parameter)
{
	list_add_tail(&self->parameters, &parameter->list);
	self->len++;
}

/**
 * hkl_parameter_list_get_values: (skip)
 * @self: the this ptr
 * @values: (array length=len): list of the paremetersc values.
 * @len: (out caller-allocates): the len of the returned list.
 *
 * get a list of all the #HklParameter values
 **/
inline void hkl_parameter_list_get_values(const HklParameterList *self,
					  double values[], uint *len)
{
	return self->ops->get_values(self, values, len);
}

/**
 * hkl_parameter_list_set_values:
 * @self: the this ptr
 * @values: (array length=len): the values to set
 * @len: the length of the values
 * @error: error set if something goes wrong
 *
 * set the parameter list with the given values
 *
 * Return value: true if succeed or false otherwise
 **/
inline unsigned int hkl_parameter_list_set_values(HklParameterList *self,
						  double values[], uint len,
						  HklError **error)
{
	return self->ops->set_values(self, values, len, error);
}

/**
 * hkl_parameter_list_get_values_unit:
 * @self: the this ptr
 * @len: (out caller-allocates): the length of the returned array
 *
 * Return value: (array length=len) (transfer full): list of pseudo axes values with unit
 *               free the array with free when done 
 **/
inline double *hkl_parameter_list_get_values_unit(const HklParameterList *self,
						  unsigned int *len)
{
	return self->ops->get_values_unit(self, len);
}

/**
 * hkl_parameter_list_set_values_unit:
 * @self: the this ptr
 * @values: (array length=len): the values to set
 * @len: the length of the values
 * @error: error set if something goes wrong
 *
 * set the parameter list with the given values
 *
 * Return value: true if succeed or false otherwise
 **/
inline unsigned int hkl_parameter_list_set_values_unit(HklParameterList *self,
						       double values[], uint len,
						       HklError **error)
{
	return self->ops->set_values_unit(self, values, len, error);
}
