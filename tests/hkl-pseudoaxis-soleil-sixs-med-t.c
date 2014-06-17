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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include "hkl-axis-private.h" /* temporary */

#define GET_GAMMA(geometries) hkl_parameter_value_unit_get(		\
		hkl_geometry_axis_get(					\
			hkl_geometry_list_item_geometry_get(		\
				hkl_geometry_list_items_first_get((geometries))), \
			"gamma"))

static void qper_qpar(void)
{
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double qper_qpar[2];
	double gamma;
	const HklGeometryList *geometries;
	HklMatrix *U;

	factory = hkl_factory_get_by_name("SOLEIL SIXS MED2+3");
	geom = hkl_factory_create_new_geometry(factory);

	sample = hkl_sample_new("test");
	U = hkl_matrix_new_euler(-90.0 * HKL_DEGTORAD, 0., 0.);
	hkl_sample_U_set(sample, U);
	hkl_matrix_free(U);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries_get(engines);

	engine = hkl_engine_list_engine_get_by_name(engines, "qper_qpar");

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 0., 0.1, 0., 0., 90., 0.);
	hkl_engine_initialize(engine, NULL);

	/* gamma must be positif */
	qper_qpar[0] = 0.1;
	qper_qpar[1] = 4.;
	hkl_engine_pseudo_axes_values_set(engine, qper_qpar, ARRAY_SIZE(qper_qpar), NULL);
	if(hkl_engine_set(engine, NULL) == TRUE){
		gamma = GET_GAMMA(geometries);
		is_double(2.61077, gamma, HKL_EPSILON * 10, __func__);
	}

	/* gamma must be negatif */
	qper_qpar[0] = -0.1;
	qper_qpar[1] = 4.;
	hkl_engine_pseudo_axes_values_set(engine, qper_qpar, ARRAY_SIZE(qper_qpar), NULL);
	if(hkl_engine_set(engine, NULL) == TRUE){
		gamma = GET_GAMMA(geometries);
		is_double(-2.7956354, gamma, HKL_EPSILON * 10, __func__);
	}

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(2);

	qper_qpar();

	return 0;
}
