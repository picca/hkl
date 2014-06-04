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
#ifndef __HKL_DETECTOR_PRIVATE_H__
#define __HKL_DETECTOR_PRIVATE_H__

#include <stddef.h>                     // for size_t
#include "hkl-geometry-private.h"       // for HklHolder
#include "hkl-vector-private.h"         // for HklVector
#include "hkl.h"                        // for HklDetector, etc

HKL_BEGIN_DECLS

struct _HklDetector
{
	size_t idx;
	HklHolder const *holder;
};

extern HklDetector *hkl_detector_new(void);

extern HklDetector *hkl_detector_new_copy(HklDetector const *src) HKL_ARG_NONNULL(1);

extern void hkl_detector_attach_to_holder(HklDetector *self,
					  HklHolder const *holder) HKL_ARG_NONNULL(1, 2);

extern int hkl_detector_compute_kf(HklDetector const *self, HklGeometry *g,
				   HklVector *kf) HKL_ARG_NONNULL(1, 2, 3);

HKL_END_DECLS

#endif /* __HKL_DETECTOR_PRIVATE_H__ */
