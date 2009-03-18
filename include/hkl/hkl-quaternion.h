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

extern void hkl_quaternion_fprintf(FILE *file, HklQuaternion const *self);

/*!
 *\brief This constructor creates a quaternion and populates it
 *\param v
 *
 *Create a new quaternion from a vector.
 */
extern void hkl_quaternion_from_vector(HklQuaternion *self, HklVector const *v);

/**
 *\brief This constructor creates a quaternion from an angle and a vector
 *\param angle the rotation angle.
 *\param v the axe of the rotation.
 *
 * inline to speed the computation.
 */
extern void hkl_quaternion_from_angle_and_axe(HklQuaternion *self,
					      double angle, HklVector const *v);

extern int hkl_quaternion_cmp(HklQuaternion const *self, HklQuaternion const *q);

extern void hkl_quaternion_minus_quaternion(HklQuaternion *self,
					    HklQuaternion const *q);

/** 
 * @brief multiplication of two quaternions
 * 
 * @param q 
 * @param q1 
 *
 * inline to speed the computation.
 */
extern void hkl_quaternion_times_quaternion(HklQuaternion *self,
					    HklQuaternion const *q);

extern double hkl_quaternion_norm2(HklQuaternion const *self);

extern void hkl_quaternion_conjugate(HklQuaternion *self);

/*!
 *\brief Compute the rotation matrix of a Quaternion.
 *\return The rotation matrix of a Quaternion.
 *
 *to convert a quaternion to a Matrix:
 *\f$ q = a + b \cdot i + c \cdot j + d \cdot k \f$
 *
 *\f$
 *\left(
 *  \begin{array}{ccc}
 *    a^2+b^2-c^2-d^2 & 2bc-2ad         & 2ac+2bd\\
 *    2ad+2bc         & a^2-b^2+c^2-d^2 & 2cd-2ab\\
 *    2bd-2ac         & 2ab+2cd         & a^2-b^2-c^2+d^2
 *  \end{array}
 *\right)
 *\f$
 */
extern void hkl_quaternion_to_smatrix(HklQuaternion const *self, HklMatrix *m);

/*!
 *\brief Decompose a Quaternion into a rotation angle and an Axe of rotation.
 *\param[out] angle The angle of the rotation will be strore in this variable.
 *\param[out] axe The axe of rotation will be store in this variable.
 */
extern void hkl_quaternion_to_angle_and_axe(HklQuaternion const *self,
					    double *angle, HklVector *v);

HKL_END_DECLS

#endif
