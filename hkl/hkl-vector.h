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

extern void hkl_vector_fprintf(FILE *file, const HklVector *self);

extern int hkl_vector_cmp(const HklVector *self, const HklVector *vector);

extern int hkl_vector_is_opposite(const HklVector *self,
				  const HklVector *vector);

extern void hkl_vector_add_vector(HklVector *self, const HklVector *vector);

extern void hkl_vector_minus_vector(HklVector *self, const HklVector *vector);

extern void hkl_vector_div_double(HklVector *self, const double d);

extern void hkl_vector_times_double(HklVector *self, const double d);

extern void hkl_vector_times_vector(HklVector *self, const HklVector *vector);

extern void hkl_vector_times_matrix(HklVector *self, const HklMatrix *m);

extern double hkl_vector_sum(const HklVector *self);

extern double hkl_vector_scalar_product(const HklVector *self,
					const HklVector *vector);

extern void hkl_vector_vectorial_product(HklVector *self,
					 const HklVector *vector);

extern double hkl_vector_angle(const HklVector *self,
			       const HklVector *vector);

extern double hkl_vector_oriented_angle(const HklVector *self,
					const HklVector *vector,
					const HklVector *ref);

extern double hkl_vector_oriented_angle_points(const HklVector *self,
					       const HklVector *p2,
					       const HklVector *p3,
					       const HklVector *ref);

extern double hkl_vector_norm2(const HklVector *self);

extern int hkl_vector_normalize(HklVector *self);

extern int hkl_vector_is_colinear(const HklVector *self,
				  const HklVector *vector);

extern void hkl_vector_randomize(HklVector *self);

extern void hkl_vector_randomize_vector(HklVector *self,
					const HklVector *vector);

extern void hkl_vector_randomize_vector_vector(HklVector *self,
					       const HklVector *vector1,
					       const HklVector *vector2);

extern void hkl_vector_rotated_around_vector(HklVector *self,
					     const HklVector *axe,
					     double angle);

extern void hkl_vector_rotated_quaternion(HklVector *self,
					  const HklQuaternion *qr);

extern void hkl_vector_rotated_around_line(HklVector *self, double angle,
					   const HklVector *c1, const HklVector *c2);

extern int hkl_vector_is_null(const HklVector *self);

extern void hkl_vector_project_on_plan(HklVector *self,
				       const HklVector *plan,
				       const HklVector *point);

HKL_END_DECLS

#endif
