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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_SOURCE_H__
#define __HKL_SOURCE_H__

#include <hkl/hkl-vector.h>

HKL_BEGIN_DECLS

#define HKL_SOURCE_DEFAULT_WAVE_LENGTH (1.54)

typedef struct _HklSource HklSource;

struct _HklSource
{
	double wave_length;
	HklVector direction;
};

extern int hkl_source_init(HklSource *self, double wave_length,
			   double x, double y, double z);

extern int hkl_source_cmp(HklSource const *self, HklSource const *s);

extern void hkl_source_compute_ki(HklSource const *self, HklVector *ki);

extern double hkl_source_get_wavelength(HklSource const *self);

extern void hkl_source_fprintf(FILE *f, HklSource const *self);

HKL_END_DECLS

#endif /* __HKL_SOURCE_H__ */
