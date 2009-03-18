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
#ifndef __HKL_LATTICE_H__
#define __HKL_LATTICE_H__

#include <hkl/hkl-parameter.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

typedef struct _HklLattice HklLattice;

struct _HklLattice
{
	HklParameter *a;
	HklParameter *b;
	HklParameter *c;
	HklParameter *alpha;
	HklParameter *beta;
	HklParameter *gamma;
};

extern HklLattice *hkl_lattice_new(double a, double b, double c,
				   double alpha, double beta, double gamma);
extern HklLattice *hkl_lattice_new_copy(HklLattice const *self);
extern HklLattice *hkl_lattice_new_default(void);

extern void hkl_lattice_free(HklLattice *self);

extern int hkl_lattice_set(HklLattice *self,
			   double a, double b, double c,
			   double alpha, double beta, double gamma);

extern int hkl_lattice_get_B(HklLattice const *self, HklMatrix *B);

extern int hkl_lattice_reciprocal(HklLattice const *self, HklLattice *r);

extern void hkl_lattice_randomize(HklLattice *self);

extern void hkl_lattice_fprintf(FILE *f, HklLattice const *self);

HKL_END_DECLS

#endif /* __HKL_LATTICE_H__ */
