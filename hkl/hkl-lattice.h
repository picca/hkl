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
#ifndef __HKL_LATTICE_H__
#define __HKL_LATTICE_H__

#include <hkl/hkl-macros.h>
#include <hkl/hkl-parameter.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

typedef struct _HklLattice HklLattice;

HKLAPI HklLattice *hkl_lattice_new(double a, double b, double c,
				   double alpha, double beta, double gamma);

HKLAPI HklLattice *hkl_lattice_new_copy(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI HklLattice *hkl_lattice_new_default(void);

HKLAPI void hkl_lattice_free(HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI const HklParameter *hkl_lattice_a_get(const HklLattice *self) HKL_ARG_NONNULL(1);
HKLAPI void hkl_lattice_a_set(HklLattice *self, const HklParameter *parameter) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_b_get(const HklLattice *self) HKL_ARG_NONNULL(1);
HKLAPI void hkl_lattice_b_set(HklLattice *self, const HklParameter *parameter) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_c_get(const HklLattice *self) HKL_ARG_NONNULL(1);
HKLAPI void hkl_lattice_c_set(HklLattice *self, const HklParameter *parameter) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_alpha_get(const HklLattice *self) HKL_ARG_NONNULL(1);
HKLAPI void hkl_lattice_alpha_set(HklLattice *self, const HklParameter *parameter) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_beta_get(const HklLattice *self) HKL_ARG_NONNULL(1);
HKLAPI void hkl_lattice_beta_set(HklLattice *self, const HklParameter *parameter) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_gamma_get(const HklLattice *self) HKL_ARG_NONNULL(1);
HKLAPI void hkl_lattice_gamma_set(HklLattice *self, const HklParameter *parameter) HKL_ARG_NONNULL(1, 2);


HKLAPI int hkl_lattice_set(HklLattice *self,
			   double a, double b, double c,
			   double alpha, double beta, double gamma) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_get_B(const HklLattice *self, HklMatrix *B) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_lattice_get_1_B(const HklLattice *self, HklMatrix *B) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_lattice_reciprocal(const HklLattice *self, HklLattice *reciprocal) HKL_ARG_NONNULL(1, 2);

HKL_END_DECLS

#endif /* __HKL_LATTICE_H__ */
