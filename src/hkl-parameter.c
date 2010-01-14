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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-parameter.h>

HklParameter *hkl_parameter_new(char const *name,
				double min, double value, double max,
				int fit, int changed,
				HklUnit const *unit, HklUnit const *punit)
{
	HklParameter *parameter;

	parameter = HKL_MALLOC(HklParameter);

	if (hkl_parameter_init(parameter,
			       name, min, value, max,
			       fit, changed,
			       unit, punit)) {
		free(parameter);
		parameter = NULL;
	}

	return parameter;
}

HklParameter *hkl_parameter_new_copy(HklParameter const *self)
{
	HklParameter *parameter = NULL;

	parameter = HKL_MALLOC(HklParameter);

	*parameter = *self;

	return parameter;
}

int hkl_parameter_init(HklParameter *self, char const *name,
		       double min, double value, double max,
		       int fit, int changed,
		       HklUnit const *unit, HklUnit const *punit)
{
	if (min <= value
	    && value <= max
	    && strcmp(name, "")
	    && hkl_unit_compatible(unit, punit)) {
		self->name = name;
		self->range.min = min;
		self->range.max = max;
		self->value = value;
		self->unit = unit;
		self->punit = punit;
		self->fit = fit;
		self->changed = changed;
	} else
		return HKL_FAIL;

	return HKL_SUCCESS;
}

void hkl_parameter_free(HklParameter *self)
{
	free(self);
}

void hkl_parameter_set_value(HklParameter *self, double value)
{
	self->value = value;
	self->changed = HKL_TRUE;
}

/* TODO test */
double hkl_parameter_get_value_unit(HklParameter const *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	return self->value * factor;
}

/* TODO test */
int hkl_parameter_set_value_unit(HklParameter *self, double value)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	self->value = value / factor;
	self->changed = HKL_TRUE;

	return HKL_SUCCESS;
}

double hkl_parameter_get_max(HklParameter const *self)
{
	return self->range.max;
}


/* TODO test */
void hkl_parameter_get_range_unit(HklParameter const *self, double *min, double *max)
{
	double factor = hkl_unit_factor(self->unit, self->punit);

	*min = factor * self->range.min;
	*max = factor * self->range.max;
}

/* TODO test */
void hkl_parameter_set_range(HklParameter *self, double min, double max)
{
	self->range.min = min;
	self->range.max = max;
}

/* TODO test */
void hkl_parameter_set_range_unit(HklParameter *self, double min, double max)
{
	double factor = hkl_unit_factor(self->unit, self->punit);
	self->range.min = min / factor;
	self->range.max = max / factor;
}

void hkl_parameter_randomize(HklParameter *self)
{
	if (self->fit) {
		double alea = (double)rand() / (RAND_MAX + 1.);
		self->value = self->range.min
			+ (self->range.max - self->range.min) * alea;
		self->changed = HKL_TRUE;
	}
}

int hkl_parameter_is_valid(HklParameter const *self)
{
	if(self->value < (self->range.min - HKL_EPSILON)
	   || self->value > (self->range.max + HKL_EPSILON))
		return HKL_FALSE;
	else
		return HKL_TRUE;
}

void hkl_parameter_fprintf(FILE *f, HklParameter *self)
{
	double factor = hkl_unit_factor(self->unit, self->punit);
	if (self->punit)
		fprintf(f, "\"%s\" : %.7f %s [%.7f : %.7f] (%d)",
			self->name,
			self->value * factor,
			self->punit->repr,
			self->range.min * factor,
			self->range.max * factor,
			self->fit);
	else
		fprintf(f, "\"%s\" : %.7f [%.7f : %.7f] (%d)",
			self->name,
			self->value * factor,
			self->range.min * factor,
			self->range.max * factor,
			self->fit);
}
