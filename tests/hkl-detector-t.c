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
 * Copyright (C) 2003-2016 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

#include "hkl-axis-private.h" /* temporary */
#include "hkl-detector-private.h"

static void new(void)
{
	HklDetector *detector1;
	HklDetector *detector2;

	detector1 = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	ok(1 == detector1->idx, __func__);
	ok(NULL == detector1->holder, __func__);

	detector2 = hkl_detector_new_copy(detector1);

	ok(detector1->idx == detector2->idx, __func__);
	ok(detector1->holder == detector2->holder, __func__);

	hkl_detector_free(detector1);
	hkl_detector_free(detector2);
}

static void attach_to_holder(void)
{
	HklDetector *detector = NULL;
	HklGeometry *geometry = NULL;
	HklHolder *holder = NULL;

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	geometry = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(geometry);
	hkl_detector_attach_to_holder(detector, holder);

	ok(1 == detector->idx, __func__);
	ok(holder == detector->holder, __func__);

	hkl_geometry_free(geometry);
	hkl_detector_free(detector);
}

static void compute_kf(void)
{
	int res = TRUE;
	HklDetector *detector = NULL;
	HklGeometry *geometry = NULL;
	HklHolder *holder = NULL;
	HklVector kf;
	HklVector kf_ref = {{0, HKL_TAU / HKL_SOURCE_DEFAULT_WAVE_LENGTH, 0}};

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	geometry = hkl_geometry_new(NULL);
	/* add a fake first holder */
	holder = hkl_geometry_add_holder(geometry);
	/* for now all detectors MUST be connected to the second
	 * holder. We will decide about a better API to connect
	 * geometry and detector */
	holder = hkl_geometry_add_holder(geometry);
	hkl_holder_add_rotation_axis(holder, "a", 1, 0, 0);
	hkl_holder_add_rotation_axis(holder, "b", 0, 1, 0);

	res &= DIAG(hkl_parameter_value_set(darray_item(geometry->axes, 0), M_PI_2, HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(hkl_parameter_value_set(darray_item(geometry->axes, 1), M_PI_2, HKL_UNIT_DEFAULT, NULL));

	hkl_detector_attach_to_holder(detector, holder);
	hkl_detector_compute_kf(detector, geometry, &kf);
	res &= DIAG(0 == hkl_vector_cmp(&kf_ref, &kf));

	ok(res, __func__);

	hkl_geometry_free(geometry);
	hkl_detector_free(detector);
}

int main(void)
{
	plan(7);

	new();
	attach_to_holder();
	compute_kf();

	return 0;
}
