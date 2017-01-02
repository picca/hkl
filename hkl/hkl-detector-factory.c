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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stddef.h>                     // for NULL
#include "hkl-detector-private.h"       // for hkl_detector_new
#include "hkl.h"                        // for HklDetector, etc

/**
 * hkl_detector_factory_new:
 * @type:
 *
 * Detector factory
 *
 * Returns:
 **/
HklDetector *hkl_detector_factory_new(HklDetectorType type)
{
	HklDetector *detector = NULL;

	switch(type) {
	case HKL_DETECTOR_TYPE_0D:
		detector = hkl_detector_new();
		break;
	}

	return detector;
}
