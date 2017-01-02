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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <math.h>                       // for fabs, sqrt
#include <stdio.h>                      // for fprintf, FILE
#include <stdlib.h>                     // for free
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-source-private.h"         // for HklSource
#include "hkl-vector-private.h"         // for hkl_vector_div_double, etc
#include "hkl.h"                        // for HKL_EPSILON, FALSE, etc

/**
 * hkl_source_dup:
 * @self: the #Hklsource to copy
 *
 * copy constructor
 * TODO test
 *
 * Returns:
 **/
HklSource *hkl_source_dup(const HklSource *self)
{
	HklSource *dup = HKL_MALLOC(HklSource);

	*dup = *self;

	return dup;
}

/**
 * hkl_source_free:
 * @self: the #Hklsource to delete
 *
 * destructor
 * TODO: test
 **/
void hkl_source_free(HklSource *self)
{
	if (self)
		free(self);
}

/**
 * hkl_source_init:
 * @self: the #Hklsource to initialize
 * @wave_length: the wave length to set
 * @x: x coordinates of the ki vector
 * @y: y coordinates of the ki vector
 * @z: z coordinates of the ki vector
 *
 * initialize the #HklSource
 *
 * Returns: HKL_SUCCESS if everythongs goes fine, HKL_FAIL otherwise
 **/
int hkl_source_init(HklSource *self,
		    double wave_length, double x, double y, double z)
{
	if (wave_length > HKL_EPSILON &&
	    ( x > HKL_EPSILON || y > HKL_EPSILON || z > HKL_EPSILON)) {
		double norm;

		norm = sqrt(x*x + y*y + z*z);

		self->wave_length = wave_length;
		hkl_vector_init(&self->direction, x, y, z);
		hkl_vector_div_double(&self->direction, norm);
		return TRUE;
	} else
		return FALSE;
}

/**
 * hkl_source_cmp: (skip)
 * @self: 1st #Hklsource
 * @s: 2nd #Hklsource
 *
 * compare two sources
 *
 * Returns:
 **/
int hkl_source_cmp(HklSource const *self, HklSource const *s)
{
	return ( (fabs(self->wave_length - s->wave_length) < HKL_EPSILON)
		 && hkl_vector_is_colinear(&self->direction,
					   &s->direction));
}

/**
 * hkl_source_compute_ki: (skip)
 * @self:
 * @ki: (out caller-allocates):
 *
 * compute the ki hkl_vector
 **/
void hkl_source_compute_ki(HklSource const *self, HklVector *ki)
{
	*ki = self->direction;
	hkl_vector_times_double(ki, HKL_TAU / self->wave_length);
}

/**
 * hkl_source_get_wavelength: (skip)
 * @self:
 *
 * get the wave_length
 *
 * Returns: the wave_length
 **/
double hkl_source_get_wavelength(HklSource const *self)
{
	return self->wave_length;
}

/**
 * hkl_source_fprintf: (skip)
 * @f:
 * @self:
 *
 * printf the source
 **/
void hkl_source_fprintf(FILE *f, HklSource const *self)
{
	fprintf(f, "%f", self->wave_length);
	hkl_vector_fprintf(f, &self->direction);
}
