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
#ifndef _HKL_INTERVAL_H
#define _HKL_INTERVAL_H

#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklInterval HklInterval;

struct _HklInterval
{
	double min;
	double max;
};

extern int hkl_interval_cmp(HklInterval const *self, HklInterval const *interval);

extern void hkl_interval_plus_interval(HklInterval *self, HklInterval const *interval);

extern void hkl_interval_plus_double(HklInterval *self, double const d);

extern void hkl_interval_minus_interval(HklInterval *self, HklInterval const *interval);

extern void hkl_interval_minus_double(HklInterval *self, double const d);

extern void hkl_interval_times_interval(HklInterval *self, HklInterval const *interval);

extern void hkl_interval_times_double(HklInterval *self, double const d);

extern void hkl_interval_divides_double(HklInterval *self, double const d);

extern int hkl_interval_contain_zero(HklInterval const *self);

extern void hkl_interval_cos(HklInterval *self);

extern void hkl_interval_acos(HklInterval *self);

extern void hkl_interval_sin(HklInterval *self);

extern void hkl_interval_asin(HklInterval *self);

extern void hkl_interval_tan(HklInterval *self);

extern void hkl_interval_atan(HklInterval *self);

extern double hkl_interval_length(HklInterval const *self);

extern void hkl_interval_angle_restrict_symm(HklInterval *self);

HKL_END_DECLS

#endif
