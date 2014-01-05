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
#ifndef __HKL_LATTICE_PRIVATE_H__
#define __HKL_LATTICE_PRIVATE_H__

#include <stdio.h>

#include "hkl.h"

HKL_BEGIN_DECLS

struct _HklLattice
{
	HklParameter *a;
	HklParameter *b;
	HklParameter *c;
	HklParameter *alpha;
	HklParameter *beta;
	HklParameter *gamma;
};

extern void hkl_lattice_lattice_set(HklLattice *self, const HklLattice *lattice);

extern void hkl_lattice_randomize(HklLattice *self);

extern void hkl_lattice_fprintf(FILE *f, const HklLattice *self);

HKL_END_DECLS

#endif /* __HKL_LATTICE_PRIVATE_H__ */
