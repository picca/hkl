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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>                      // for fprintf, NULL, FILE
#include <stdlib.h>                     // for free
#include "hkl-detector-private.h"       // for _HklDetector
#include "hkl-geometry-private.h"       // for HklHolder, _HklGeometry, etc
#include "hkl-macros-private.h"         // for HKL_MALLOC
#include "hkl-source-private.h"         // for HklSource
#include "hkl-vector-private.h"         // for hkl_vector_init, etc
#include "hkl.h"                        // for HklDetector, HklGeometry, etc
#include "hkl/ccan/darray/darray.h"     // for darray_item

/**
 * hkl_detector_new: (skip)
 *
 * Create a new default #HklDetector
 *
 * Returns:
 **/
HklDetector *hkl_detector_new(void)
{
	HklDetector *self = NULL;

	self = HKL_MALLOC(HklDetector);

	self->idx = 1;
	self->holder = NULL;

	return self;
}

/**
 * hkl_detector_new_copy: (skip)
 * @src: the detector to copy
 *
 * the copy constructor
 *
 * Returns:
 **/
HklDetector *hkl_detector_new_copy(const HklDetector *src)
{
	HklDetector *self;

	self = HKL_MALLOC(HklDetector);

	*self = *src;

	return self;
}

/**
 * hkl_detector_free: (skip)
 * @self:
 *
 * destructor
 **/
void hkl_detector_free(HklDetector *self)
{
	free(self);
}

/**
 * hkl_detector_attach_to_holder: (skip)
 * @self:
 * @holder:
 *
 * attach the #HklDetector to an #HklHolder
 **/
void hkl_detector_attach_to_holder(HklDetector *self, HklHolder const *holder)
{
	self->holder = holder;
}

/**
 * hkl_detector_compute_kf: (skip)
 * @self:
 * @g: (in): the diffractometer #HklGeometry use to compute kf.
 * @kf: (out caller-allocates): the #HklVector fill with the kf coordinates.
 *
 * Compute the kf vector of the #HklDetector
 *
 * Returns: HKL_SUCCESS if everythongs goes fine. HKL_FAIL otherwise.
 **/
int hkl_detector_compute_kf(HklDetector const *self, HklGeometry *g,
			    HklVector *kf)
{
	HklHolder *holder;

	hkl_geometry_update(g);

	holder = darray_item(g->holders, self->idx);
	if (holder) {
		hkl_vector_init(kf, HKL_TAU / g->source.wave_length, 0, 0);
		hkl_vector_rotated_quaternion(kf, &holder->q);
		return TRUE;
	} else
		return FALSE;
}

/**
 * hkl_detector_fprintf: (skip)
 * @f:
 * @self:
 *
 * print to a FILE the detector members
 **/
void hkl_detector_fprintf(FILE *f, const HklDetector *self)
{
	fprintf(f, "detector->idx: %d\n", self->idx);
	fprintf(f, "detector->holder: %p\n", self->holder);
}
