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
#ifndef __HKL_VECTOR_H__
#define __HKL_VECTOR_H__

#include <stdio.h>
#include <math.h>
#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

/* forward declaration begin */
typedef struct _HklMatrix HklMatrix;
typedef struct _HklQuaternion HklQuaternion;
/* forward declaration end */

typedef struct _HklVector HklVector;

struct _HklVector
{
	double data[3];
};

extern void hkl_vector_init(HklVector *self, double x, double y, double z);

extern void hkl_vector_fprintf(FILE *file, HklVector const *self);

extern int hkl_vector_cmp(HklVector const *self, HklVector const *vector);

extern int hkl_vector_is_opposite(HklVector const *self,
				  HklVector const *vector);

extern void hkl_vector_add_vector(HklVector *self, HklVector const *vector);

extern void hkl_vector_minus_vector(HklVector *self, HklVector const *vector);

extern void hkl_vector_div_double(HklVector *self, double const d);

extern void hkl_vector_times_double(HklVector *self, double const d);

extern void hkl_vector_times_vector(HklVector *self, HklVector const *vector);

extern void hkl_vector_times_smatrix(HklVector *self, HklMatrix const *m);

extern double hkl_vector_sum(HklVector const *self);

extern double hkl_vector_scalar_product(HklVector const *self,
					HklVector const *vector);

extern void hkl_vector_vectorial_product(HklVector *self,
					 HklVector const *vector);

extern double hkl_vector_angle(HklVector const *self,
			       HklVector const *vector);

extern double hkl_vector_oriented_angle(HklVector const *self,
					HklVector const *vector,
					HklVector const *ref);

extern double hkl_vector_norm2(HklVector const *self);

extern int hkl_vector_normalize(HklVector *self);

extern int hkl_vector_is_colinear(HklVector const *self,
				  HklVector const *vector);

extern void hkl_vector_randomize(HklVector *self);

extern void hkl_vector_randomize_vector(HklVector *self,
					HklVector const *vector);

extern void hkl_vector_randomize_vector_vector(HklVector *self,
					       HklVector const *vector1,
					       HklVector const *vector2);

extern void hkl_vector_rotated_around_vector(HklVector *self,
					     HklVector const *axe,
					     double angle);

extern void hkl_vector_rotated_quaternion(HklVector *self,
					  HklQuaternion const *q);

extern int hkl_vector_is_null(HklVector const *self);

extern void hkl_vector_project_on_plan(HklVector *self,
				       HklVector const *plan);

HKL_END_DECLS

#endif
