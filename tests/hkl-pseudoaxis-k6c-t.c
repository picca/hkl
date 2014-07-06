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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

static void degenerated(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const darray_string *modes;
	const char **mode;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {0, 1, 0};

	factory = hkl_factory_get_by_name("K6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	modes = hkl_engine_modes_names_get(engine);

	darray_foreach(mode, *modes){
		const darray_string *parameters;
		HklGeometryList *geometries;

		hkl_engine_current_mode_set(engine, *mode, NULL);
		parameters = hkl_engine_parameters_names_get(engine);
		if (!strcasecmp(*mode, "constant_chi_vertical")){
			size_t n_params = darray_size(*parameters);
			double params[n_params];

			hkl_engine_parameters_values_get(engine,
							 params, n_params,
							 HKL_UNIT_DEFAULT);
			params[0] = 1;
			hkl_engine_parameters_values_set(engine,
							 params, n_params,
							 HKL_UNIT_DEFAULT, NULL);
		}
		if (!strcasecmp(*mode, "constant_incidence")){
			size_t n_params = darray_size(*parameters);
			double params[n_params];

			hkl_engine_parameters_values_get(engine,
							 params, n_params,
							 HKL_UNIT_DEFAULT);
			params[3] = 1;
			hkl_engine_parameters_values_set(engine,
							 params, n_params,
							 HKL_UNIT_DEFAULT, NULL);
		}

		/* studdy this degenerated case */
		geometries = hkl_engine_pseudo_axes_values_set(engine,
								hkl, ARRAY_SIZE(hkl),
								HKL_UNIT_DEFAULT, NULL);
		if (geometries){
			const HklGeometryListItem *item;

			HKL_GEOMETRY_LIST_FOREACH(item, geometries){
				hkl_geometry_set(geometry, hkl_geometry_list_item_geometry_get(item));
				res &= check_pseudoaxes(engine, hkl, ARRAY_SIZE(hkl));
			}
			hkl_geometry_list_free(geometries);
		}
	}

	ok(res == TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void eulerians(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const darray_string *modes;
	const char **mode;
	const HklGeometryListItem *item;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("K6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "eulerians", NULL);
	modes = hkl_engine_modes_names_get(engine);

	darray_foreach(mode, *modes){
		double omega, chi, phi;
		const darray_string *parameters;
		HklGeometryList *geometries;
		size_t n_params;

		hkl_engine_current_mode_set(engine, *mode, NULL);
		parameters = hkl_engine_parameters_names_get(engine);
		n_params = darray_size(*parameters);
		if (n_params){
			double params[n_params];

			hkl_engine_parameters_values_get(engine,
							 params, n_params,
							 HKL_UNIT_DEFAULT);
			params[0] = 1;
			hkl_engine_parameters_values_set(engine,
							 params, n_params,
							 HKL_UNIT_DEFAULT, NULL);
		}

		/* studdy this degenerated case */
		geometries = hkl_engine_set_values_v(engine, 0., 90. * HKL_DEGTORAD, 0.);
		if (geometries) {
			res &= hkl_geometry_list_n_items_get(geometries) == 2;

			/* first solution = -180, -90, 180 */
			item = hkl_geometry_list_items_first_get(geometries);
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			res &= check_pseudoaxes_v(engine, -180. * HKL_DEGTORAD, -90. * HKL_DEGTORAD, 180. * HKL_DEGTORAD);

			/* second solution = 0, 90, 0 */
			item = hkl_geometry_list_items_next_get(geometries, item);
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			res &= check_pseudoaxes_v(engine, 0., 90. * HKL_DEGTORAD, 0.);

			/* no more solution */
			res &= hkl_geometry_list_items_next_get(geometries, item) == NULL;

			hkl_geometry_list_free(geometries);
		}
	}

	ok(res == TRUE, "eulerians");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void q2(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const darray_string *modes;
	const char **mode;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("K6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "q2", NULL);
	modes = hkl_engine_modes_names_get(engine);

	/* the init part */
	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 30., 0., 0., 0., 60.);
	hkl_engine_initialized_set(engine, TRUE, NULL);

	darray_foreach(mode, *modes){
		double q, alpha;

		hkl_engine_current_mode_set(engine, *mode, NULL);
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				double values[] = {q, alpha};
				HklGeometryList *geometries;

				geometries = hkl_engine_pseudo_axes_values_set(engine,
									       values, ARRAY_SIZE(values),
									       HKL_UNIT_DEFAULT, NULL);
				if(geometries){
					const HklGeometryListItem *item;

					HKL_GEOMETRY_LIST_FOREACH(item, geometries){
						hkl_geometry_set(geometry,
								 hkl_geometry_list_item_geometry_get(item));
						res &= check_pseudoaxes(engine, values, 2);
					}
					hkl_geometry_list_free(geometries);
				}
			}
	}

	ok(res == TRUE, "q2");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}


static void m15110(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("K6C", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);

	/* the init part must succed */
	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 62.95, 134.75, 0., 0., 60.);
	res &= hkl_engine_initialized_set(engine, TRUE, NULL);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);

	ok(res == TRUE, "m15110");
}

int main(int argc, char** argv)
{
	plan(4);

	degenerated();
	eulerians();
	q2();
	m15110();

	return 0;
}
