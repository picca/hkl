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
#include <math.h>
#include <hkl/hkl-detector.h>

/* public part */

int hkl_detector_compute_kf(HklDetector const *self, HklGeometry *g,
			    HklVector *kf)
{
	hkl_geometry_update(g);

	HklHolder *holder = &g->holders[self->idx];
	if (holder) {
		hkl_vector_init(kf, HKL_TAU / g->source.wave_length, 0, 0);
		hkl_vector_rotated_quaternion(kf, &holder->q);
		return HKL_SUCCESS;
	} else
		return HKL_FAIL;
}
