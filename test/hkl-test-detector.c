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

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME detector

HKL_TEST_SUITE_FUNC(compute_kf)
{
	HklDetector det = {0};
	HklGeometry *geom = NULL;
	HklAxis *axis1 = NULL;
	HklAxis *axis2 = NULL;
	HklHolder *holder = NULL;
	HklVector kf;
	HklVector kf_ref = {{0, HKL_TAU / HKL_SOURCE_DEFAULT_WAVE_LENGTH, 0}};

	geom = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "b", 0, 1, 0);

	hkl_axis_set_value(&geom->axes[0], M_PI_2);
	hkl_axis_set_value(&geom->axes[1], M_PI_2);

	hkl_detector_compute_kf(&det, geom, &kf);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&kf_ref, &kf));

	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( compute_kf );

HKL_TEST_SUITE_END
