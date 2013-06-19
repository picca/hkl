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
#ifndef __HKL_MATRIX_H__
#define __HKL_MATRIX_H__

#include <stdio.h>

#include <hkl/hkl-macros.h>
#include <hkl/hkl-vector.h>

HKL_BEGIN_DECLS

HKLAPI HklMatrix *hkl_matrix_new(void);

HKLAPI HklMatrix *hkl_matrix_new_full(double m11, double m12, double m13,
				      double m21, double m22, double m23,
				      double m31, double m32, double m33);

HKLAPI HklMatrix *hkl_matrix_new_euler(double euler_x, double euler_y, double euler_z);

HKLAPI double hkl_matrix_get(const HklMatrix *self, unsigned int i, unsigned int j) HKL_ARG_NONNULL(1);

HKLAPI void hkl_matrix_free(HklMatrix *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_matrix_init(HklMatrix *self,
			    double m11, double m12, double m13,
			    double m21, double m22, double m23,
			    double m31, double m32, double m33) HKL_ARG_NONNULL(1);

/* HKLAPI void hkl_matrix_init_from_euler(HklMatrix *self, */
/* 				       double euler_x, double euler_y, double euler_z) HKL_ARG_NONNULL(1); */

HKLAPI int hkl_matrix_cmp(const HklMatrix *self, const HklMatrix *m) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_matrix_times_matrix(HklMatrix *self, const HklMatrix *m) HKL_ARG_NONNULL(1, 2);

HKL_END_DECLS

#endif
