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
#ifndef __HKL_DETECTOR_H__
#define __HKL_DETECTOR_H__

#include <stdio.h>

#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklDetector HklDetector;

HKLAPI void hkl_detector_free(HklDetector *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_detector_idx_set(HklDetector *self, int idx) HKL_ARG_NONNULL(1);

HKLAPI void hkl_detector_fprintf(FILE *f, const HklDetector *self) HKL_ARG_NONNULL(1, 2);

HKL_END_DECLS

#endif /* __HKL_DETECTOR_H__ */
