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
#ifndef __HKL_QUATERNION_H__
#define __HKL_QUATERNION_H__

#include <stdio.h>
#include <math.h>
#include <hkl/hkl-macros.h>
#include <hkl/hkl-vector.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

struct _HklQuaternion
{
	double data[4];
};

extern void hkl_quaternion_init(HklQuaternion *self,
				double a, double b, double c, double d);

extern void hkl_quaternion_init_from_vector(HklQuaternion *self, const HklVector *v);

extern void hkl_quaternion_init_from_angle_and_axe(HklQuaternion *self,
						   double angle, const HklVector *v);

extern void hkl_quaternion_fprintf(FILE *file, const HklQuaternion *self);

extern int hkl_quaternion_cmp(const HklQuaternion *self, const HklQuaternion *q);

extern void hkl_quaternion_minus_quaternion(HklQuaternion *self,
					    const HklQuaternion *q);

extern void hkl_quaternion_times_quaternion(HklQuaternion *self,
					    const HklQuaternion *q);

extern double hkl_quaternion_norm2(const HklQuaternion *self);

extern void hkl_quaternion_conjugate(HklQuaternion *self);

extern void hkl_quaternion_to_matrix(const HklQuaternion *self, HklMatrix *m);

extern void hkl_quaternion_to_angle_and_axe(const HklQuaternion *self,
					    double *angle, HklVector *v);

HKL_END_DECLS

#endif
