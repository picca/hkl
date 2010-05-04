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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME detector

HKL_TEST_SUITE_FUNC(new)
{
	HklDetector *detector1;
	HklDetector *detector2;

	detector1 = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HKL_ASSERT_EQUAL(0, detector1->idx);
	HKL_ASSERT_POINTER_EQUAL(NULL, detector1->holder);

	detector2 = hkl_detector_new_copy(detector1);

	HKL_ASSERT_EQUAL(detector1->idx, detector2->idx);
	HKL_ASSERT_POINTER_EQUAL(detector1->holder, detector2->holder);

	hkl_detector_free(detector1);
	hkl_detector_free(detector2);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(attach_to_holder)
{
	HklDetector *detector = NULL;
	HklGeometry *geometry = NULL;
	HklHolder *holder = NULL;

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	geometry = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geometry);
	hkl_detector_attach_to_holder(detector, holder);

	HKL_ASSERT_EQUAL(0, detector->idx);
	HKL_ASSERT_POINTER_EQUAL(holder, detector->holder);

	hkl_geometry_free(geometry);
	hkl_detector_free(detector);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(compute_kf)
{
	HklDetector *detector = NULL;
	HklGeometry *geometry = NULL;
	HklAxis *axis1 = NULL;
	HklAxis *axis2 = NULL;
	HklHolder *holder = NULL;
	HklVector kf;
	HklVector kf_ref = {{0, HKL_TAU / HKL_SOURCE_DEFAULT_WAVE_LENGTH, 0}};

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	geometry = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geometry);
	hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "b", 0, 1, 0);

	hkl_axis_set_value(&geometry->axes[0], M_PI_2);
	hkl_axis_set_value(&geometry->axes[1], M_PI_2);

	hkl_detector_attach_to_holder(detector, holder);
	hkl_detector_compute_kf(detector, geometry, &kf);
	HKL_ASSERT_EQUAL(0, hkl_vector_cmp(&kf_ref, &kf));

	hkl_geometry_free(geometry);
	hkl_detector_free(detector);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( attach_to_holder );
HKL_TEST( compute_kf );

HKL_TEST_SUITE_END
