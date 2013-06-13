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
#ifndef __HKL_SAMPLE_H__
#define __HKL_SAMPLE_H__

#include <stdbool.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-geometry.h>
#include <hkl/hkl-lattice.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

typedef struct _HklSample HklSample;
typedef struct _HklSampleReflection HklSampleReflection;

/*************/
/* HklSample */
/*************/

HKLAPI HklSample *hkl_sample_new(const char *name) HKL_ARG_NONNULL(1);

HKLAPI HklSample *hkl_sample_new_copy(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_free(HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_sample_name_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_name_set(HklSample *self, const char *name) HKL_ARG_NONNULL(1, 2);

HKLAPI HklLattice *hkl_sample_lattice_get(HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_lattice_set(HklSample *self, HklLattice *lattice) HKL_ARG_NONNULL(1, 2);

HKLAPI HklParameter *hkl_sample_ux_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_ux_set(HklSample *self, const HklParameter *ux) HKL_ARG_NONNULL(1, 2);

HKLAPI HklParameter *hkl_sample_uy_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_uy_set(HklSample *self, const HklParameter *uy) HKL_ARG_NONNULL(1, 2);

HKLAPI HklParameter *hkl_sample_uz_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_uz_set(HklSample *self, const HklParameter *uz) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklMatrix *hkl_sample_U_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_U_set(HklSample *self, const HklMatrix *U) HKL_ARG_NONNULL(1);

HKLAPI const HklMatrix *hkl_sample_UB_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI double hkl_sample_UB_set(HklSample *self, const HklMatrix *UB) HKL_ARG_NONNULL(1, 2);

HKLAPI HklSampleReflection *hkl_sample_first_reflection_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI HklSampleReflection *hkl_sample_next_reflection_get(const HklSample *self,
							   const HklSampleReflection *reflection) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_sample_del_reflection(HklSampleReflection *reflection) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_add_reflection(HklSample *self,
				      HklSampleReflection *reflection) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_sample_compute_UB_busing_levy(HklSample *self,
					     const HklSampleReflection *r1,
					     const HklSampleReflection *r2) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI double hkl_sample_get_reflection_mesured_angle(const HklSample *self,
						      const HklSampleReflection *r1,
						      const HklSampleReflection *r2) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI double hkl_sample_get_reflection_theoretical_angle(const HklSample *self,
							  const HklSampleReflection *r1,
							  const HklSampleReflection *r2) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI double hkl_sample_affine(HklSample *self) HKL_ARG_NONNULL(1);

/***********************/
/* hklSampleReflection */
/***********************/

HKLAPI HklSampleReflection *hkl_sample_reflection_new(HklGeometry *geometry,
						      const HklDetector *detector,
						      double h, double k, double l) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_sample_reflection_hkl_get(const HklSampleReflection *self,
					  double *h, double *k, double *l) HKL_ARG_NONNULL(1, 2, 3, 4);

HKLAPI void hkl_sample_reflection_hkl_set(HklSampleReflection *self,
					  double h, double k, double l) HKL_ARG_NONNULL(1);

HKLAPI bool hkl_sample_reflection_flag_get(const HklSampleReflection *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_reflection_flag_set(HklSampleReflection *self, bool flag) HKL_ARG_NONNULL(1);

HKLAPI HklGeometry *hkl_sample_reflection_geometry_get(HklSampleReflection *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_reflection_geometry_set(HklSampleReflection *self,
					       const HklGeometry *geometry) HKL_ARG_NONNULL(1, 2);

HKL_END_DECLS

#endif
