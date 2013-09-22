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
#ifndef __HKL_SAMPLE_PRIVATE_H__
#define __HKL_SAMPLE_PRIVATE_H__

#include <stdio.h>                      // for FILE
#include "ccan/list/list.h"             // for list_head, list_node
#include "hkl-matrix-private.h"         // for _HklMatrix
#include "hkl-vector-private.h"         // for HklVector
#include "hkl.h"                        // for HklParameter, etc

HKL_BEGIN_DECLS

/*************/
/* HklSample */
/*************/

struct _HklSample {
	char *name;
	HklLattice *lattice;
	HklMatrix U;
	HklMatrix UB;
	HklParameter *ux;
	HklParameter *uy;
	HklParameter *uz;
	struct list_head reflections;
};

extern void hkl_sample_fprintf(FILE *f, const HklSample *self);


/***********************/
/* hklSampleReflection */
/***********************/

struct _HklSampleReflection {
	HklGeometry *geometry;
	HklDetector *detector;
	HklVector hkl;
	HklVector _hkl;
	int flag;
	struct list_node list;
};

extern HklSampleReflection *hkl_sample_reflection_new_copy(const HklSampleReflection *self);

extern void hkl_sample_reflection_free(HklSampleReflection *self);

HKL_END_DECLS

#endif
