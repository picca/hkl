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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

static void getter(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geom = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	/* geometry -> pseudo */
	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 30., 0., 0., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 0., 0., 1.));

	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 30., 0., 90., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 1., 0., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 30., 0., -90., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, -1., 0., 0.));

	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 30., 0., 180., 60.));
	res &= DIAG(check_pseudoaxes_v(engine, 0., 0., -1.));

	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 45., 0., 135., 90.));
	res &= DIAG(check_pseudoaxes_v(engine, 1., 0., -1.));

	ok(res == TRUE, "getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void degenerated(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const char **mode;
	const darray_string *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);
	modes = hkl_engine_modes_names_get(engine);

	darray_foreach(mode, *modes){
		static double values[] = {0, 0, 1};
		HklGeometryList *geometries;
		size_t n_params;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		const darray_string *parameters = hkl_engine_parameters_names_get(engine);
		n_params = darray_size(*parameters);
		if (n_params){
			double params[n_params];

			hkl_engine_parameters_values_get(engine, params, n_params, HKL_UNIT_DEFAULT);
			params[0] = 0;
			res &= DIAG(hkl_engine_parameters_values_set(engine, params, n_params, HKL_UNIT_DEFAULT, NULL));
		}

		/* studdy this degenerated case */
		geometries = hkl_engine_pseudo_axis_values_set(engine, values, 3, HKL_UNIT_DEFAULT, NULL);
		if(geometries){
			const HklGeometryListItem *item;

			HKL_GEOMETRY_LIST_FOREACH(item, geometries){
				hkl_geometry_set(geometry,
						 hkl_geometry_list_item_geometry_get(item));
				res &= DIAG(check_pseudoaxes(engine, values, ARRAY_SIZE(values)));
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

static void psi_getter(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	double hkl[3];

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geom = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);

	/* the getter part */
	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_USER, NULL, 30., 0., 0., 60.));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	/* 1 0 0 */
	hkl[0] = 1, hkl[1] = 0, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, 0.));

	/* 0 1 0*/
	hkl[0] = 0, hkl[1] = 1, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, 90. * HKL_DEGTORAD));

	/* here Q and <h, k, l>_ref are colinear must FAIL */
	hkl[0] = 0, hkl[1] = 0, hkl[2] = 1;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(!check_pseudoaxes_v(engine, 0. * HKL_DEGTORAD));

	/* -1 0 0 */
	hkl[0] = -1, hkl[1] = 0, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, 180. * HKL_DEGTORAD));

	/* 0 -1 0 */
	hkl[0] = 0, hkl[1] = -1, hkl[2] = 0;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(check_pseudoaxes_v(engine, -90. * HKL_DEGTORAD));

	/* Q and <h, k, l>_ref are colinear so must FAIL */
	hkl[0] = 0, hkl[1] = 0, hkl[2] = -1;
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(!check_pseudoaxes_v(engine, 0. * HKL_DEGTORAD));

	ok(res == TRUE, "psi getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void psi_setter(void)
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
	static double hkl[] = {1, 0, 0};

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi", NULL);
	modes = hkl_engine_modes_names_get(engine);

	/* the init part */
	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.));
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl, ARRAY_SIZE(hkl), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	darray_foreach(mode, *modes){
		double psi;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		for(psi=-180 * HKL_DEGTORAD;psi<180 * HKL_DEGTORAD;psi += HKL_DEGTORAD){
			HklGeometryList *geometries;

			geometries = hkl_engine_pseudo_axis_values_set(engine, &psi, 1,
									HKL_UNIT_DEFAULT, NULL);
			if(geometries){
				const HklGeometryListItem *item;

				HKL_GEOMETRY_LIST_FOREACH(item, geometries){
					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(item));
					res &= DIAG(check_pseudoaxes_v(engine, psi));
				}
				hkl_geometry_list_free(geometries);
			}
		}
	}

	ok(res == TRUE, "psi setter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void q(void)
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

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "q", NULL);
	modes = hkl_engine_modes_names_get(engine);

	/* the init part */
	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	darray_foreach(mode, *modes){
		double q;

		res &= DIAG(hkl_engine_current_mode_set(engine, *mode, NULL));
		for(q=-1.; q<1.; q += 0.1){
			HklGeometryList *geometries;

			geometries = hkl_engine_pseudo_axis_values_set(engine, &q, 1,
									HKL_UNIT_DEFAULT, NULL);
			if(geometries){
				const HklGeometryListItem *item;

				HKL_GEOMETRY_LIST_FOREACH(item, geometries){
					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(item));
					res &= DIAG(check_pseudoaxes(engine, &q, 1));
				}
				hkl_geometry_list_free(geometries);
			}
		}
	}

	ok(res == TRUE, "q");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void hkl_psi_constant_vertical(void)
{
	int res = TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 1};
	static double hkl2[] = {1, 1, 0};

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl", NULL);

	res &= DIAG(hkl_engine_current_mode_set(engine, "psi_constant", NULL));

	/* the init part */
	res &= DIAG(hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.));
	res &= DIAG(hkl_engine_parameters_values_set(engine, hkl2, ARRAY_SIZE(hkl2), HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));

	geometries = hkl_engine_pseudo_axis_values_set(engine, hkl, ARRAY_SIZE(hkl),
							HKL_UNIT_DEFAULT, NULL);
	if(geometries){
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, geometries){
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));
			res &= DIAG(check_pseudoaxes(engine, hkl, ARRAY_SIZE(hkl)));
		}
		hkl_geometry_list_free(geometries);
	}

	ok(res == TRUE, "psi constant vertical");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

int main(void)
{
	plan(6);

	getter();
	degenerated();
	psi_getter();
	psi_setter();
	q();
	hkl_psi_constant_vertical();

	return 0;
}
