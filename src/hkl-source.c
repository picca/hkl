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
#include <math.h>

#include <hkl/hkl-source.h>

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
		return HKL_SUCCESS;
	} else
		return HKL_FAIL;
}

/** compare two sources */
int hkl_source_cmp(HklSource const *self, HklSource const *s)
{
	return ( (fabs(self->wave_length - s->wave_length) < HKL_EPSILON)
		 && hkl_vector_is_colinear(&self->direction,
					   &s->direction));
}

/** compute the ki hkl_vector */
void hkl_source_compute_ki(HklSource const *self, HklVector *ki)
{
	*ki = self->direction;
	hkl_vector_times_double(ki, HKL_TAU / self->wave_length);
}

double hkl_source_get_wavelength(HklSource const *self)
{
	return self->wave_length;
}

/** printf the source */
void hkl_source_fprintf(FILE *f, HklSource const *self)
{
	fprintf(f, "%f", self->wave_length);
	hkl_vector_fprintf(f, &self->direction);
}
