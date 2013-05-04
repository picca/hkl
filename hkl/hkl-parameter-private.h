
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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PARAMETER_PRIVATE_H__
#define __HKL_PARAMETER_PRIVATE_H__

#include <hkl/hkl-parameter.h>

HKL_BEGIN_DECLS

/****************/
/* HklParameter */
/****************/

struct _HklParameterOperations {
	HklParameter * (*copy)(const HklParameter *self);
	void           (*free)(HklParameter *self);
	void           (*init_copy)(HklParameter *self, const HklParameter *src);
	double         (*get_value_closest)(const HklParameter *self,
				    const HklParameter *other);
	unsigned int   (*set_value)(HklParameter *self, double value,
				  HklError **error);
	unsigned int   (*set_value_unit)(HklParameter *self, double value,
				       HklError **error);
	void           (*set_value_smallest_in_range)(HklParameter *self);
	void           (*randomize)(HklParameter *self);
	int            (*is_valid)(const HklParameter *self);
	void           (*fprintf)(FILE *f, const HklParameter *self);
};

#define HKL_PARAMETER_OPERATIONS_DEFAULTS				\
	.copy = hkl_parameter_copy_real,				\
		.free = hkl_parameter_free_real,			\
		.init_copy = hkl_parameter_init_copy_real,		\
		.get_value_closest = hkl_parameter_get_value_closest_real, \
		.set_value = hkl_parameter_set_value_real,		\
		.set_value_unit = hkl_parameter_set_value_unit_real,	\
		.set_value_smallest_in_range = hkl_parameter_set_value_smallest_in_range_real, \
		.randomize = hkl_parameter_randomize_real,		\
		.is_valid = hkl_parameter_is_valid_real,		\
		.fprintf = hkl_parameter_fprintf_real

static inline HklParameter *hkl_parameter_copy_real(const HklParameter *self)
{
	HklParameter *dup = HKL_MALLOC(HklParameter);

	*dup = *self;

	return dup;
}

static inline void hkl_parameter_free_real(HklParameter *self)
{
	free(self);
}

static inline void hkl_parameter_init_copy_real(HklParameter *self, const HklParameter *src)
{
	*self = *src;
	self->changed = HKL_TRUE;
}

static inline double hkl_parameter_get_value_closest_real(const HklParameter *self,
							  const HklParameter *ref)
{
	return self->_value;
}

static inline unsigned int hkl_parameter_set_value_real(
	HklParameter *self, double value,
	HklError **error)
{
	self->_value = value;
	self->changed = HKL_TRUE;

	return HKL_TRUE;
}

static inline unsigned int hkl_parameter_set_value_unit_real(
	HklParameter *self, double value,
	HklError **error)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	return hkl_parameter_set_value_real(self, value / factor, error);
}

static inline void hkl_parameter_set_value_smallest_in_range_real(HklParameter *self)
{
	/* DOES NOTHING for a standard parameter */
}

static inline void hkl_parameter_randomize_real(HklParameter *self)
{
	if (self->fit) {
		double alea = (double)rand() / (RAND_MAX + 1.);
		self->_value = self->range.min
			+ (self->range.max - self->range.min) * alea;
		self->changed = HKL_TRUE;
	}
}

static inline int hkl_parameter_is_valid_real(const HklParameter *self)
{
	if(self->_value < (self->range.min - HKL_EPSILON)
	   || self->_value > (self->range.max + HKL_EPSILON))
		return HKL_FALSE;
	else
		return HKL_TRUE;
}

static inline void hkl_parameter_fprintf_real(FILE *f, const HklParameter *self)
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

static HklParameterOperations hkl_parameter_operations_defaults = {
	HKL_PARAMETER_OPERATIONS_DEFAULTS,
};


/********************/
/* HklParameterList */
/********************/

struct _HklParameterListOperations {
	void (*get_values)(const HklParameterList *self, double values[], unsigned int *len);
	unsigned int (*set_values)(HklParameterList *self, double values[], unsigned int len,
				   HklError **error);
	double *(*get_values_unit)(const HklParameterList *self, unsigned int *len);
	unsigned int (*set_values_unit)(HklParameterList *self, double values[], unsigned int len,
					HklError **error);
};

#define HKL_PARAMETER_LIST_OPERATIONS_DEFAULTS				\
	.get_values = hkl_parameter_list_get_values_real,		\
		.set_values = hkl_parameter_list_set_values_real,	\
		.get_values_unit = hkl_parameter_list_get_values_unit_real, \
		.set_values_unit = hkl_parameter_list_set_values_unit_real

static inline void hkl_parameter_list_get_values_real(
	const HklParameterList *self,
	double values[], unsigned int *len)
{
	for(unsigned int i; i<darray_size(*self); ++i)
		values[i] = darray_item(*self, i)->_value;

	*len = darray_size(*self);
}

static inline unsigned int hkl_parameter_list_set_values_real(
	HklParameterList *self,
	double values[], unsigned int len,
	HklError **error)
{
	unsigned int n = len < darray_size(*self) ? len : darray_size(*self);

	for(unsigned int i=0; i<n; ++i)
		if(!hkl_parameter_set_value(darray_item(*self, i),
					    values[i], error))
			return HKL_FALSE;

	return HKL_TRUE;
}

static inline double *hkl_parameter_list_get_values_unit_real(
	const HklParameterList *self,
	unsigned int *len)
{
	const unsigned int _len =  darray_size(*self);
	double *values = (double *)malloc(sizeof(*values) * _len);

	for(unsigned int i=0; i<_len; ++i)
		values[i] = hkl_parameter_get_value_unit(darray_item(*self, i));
	*len = _len;

	return values;
}

static inline unsigned int hkl_parameter_list_set_values_unit_real(
	HklParameterList *self,
	double values[], unsigned int len,
	HklError **error)
{
	for(unsigned int i=0; i<darray_size(*self); ++i)
		if(!hkl_parameter_set_value_unit(
			   darray_item(*self, i), values[i], error))
			return HKL_FALSE;

	return HKL_TRUE;
}

static HklParameterListOperations hkl_parameter_list_operations_defaults = {
	HKL_PARAMETER_LIST_OPERATIONS_DEFAULTS,
};

static void hkl_parameter_list_init(HklParameterList *self,
				    const HklParameterListOperations *ops)
{
	darray_init(*self);
	self->ops = ops;
}

static void hkl_parameter_list_release(HklParameterList *self)
{
	HklParameter **parameter;

	darray_foreach(parameter, *self){
		hkl_parameter_free(*parameter);
	}
	darray_free(*self);
}

static void hkl_parameter_list_fprintf(FILE *f, const HklParameterList *self)
{
	HklParameter **parameter;

	darray_foreach(parameter, *self){
		fprintf(f, "\n     ");
		hkl_parameter_fprintf(f, *parameter);
	}
}

extern void hkl_parameter_list_add_parameter(HklParameterList *self,
					     HklParameter *parameter);


/* this method is used to set the Parameter list without triggering
 * the public API machinery, usually this method is used in the
 * HklPseudoAxis get method to set the pseudo axis values */
static inline void _hkl_parameter_list_set_values(
	HklParameterList *self,
	double values[], unsigned int len)
{
	for(unsigned int i=0; i<darray_size(*self); ++i)
		darray_item(*self, i)->_value = values[i];
}

HKL_END_DECLS

#endif /* __HKL_PARAMETER_PRIVATE_H__ */
