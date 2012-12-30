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
#include <tap/basic.h>
#include <tap/hkl.h>

static void degenerated(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {0, 1, 0};
	HklParameterList *pseudo_axes;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "hkl");
	modes = hkl_engine_modes(engine);
	pseudo_axes = hkl_engine_pseudo_axes(engine);

	darray_foreach(mode, *modes) {
		hkl_engine_select_mode(engine, *mode);
		if(darray_size((*mode)->parameters))
			hkl_parameter_set_value(darray_item((*mode)->parameters, 0), 1, NULL);

		/* studdy this degenerated case */
		hkl_parameter_list_set_values(pseudo_axes, hkl, ARRAY_SIZE(hkl), NULL);
		if (hkl_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&geometries->items, item, node) {
				static double null[] = {0, 0, 0};

				hkl_parameter_list_set_values(pseudo_axes,
							      null, ARRAY_SIZE(null),
							      NULL);
				hkl_geometry_init_geometry(geom, item->geometry);
				hkl_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, hkl, 3);
			}
		}
	}

	ok(res == HKL_TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void eulerians(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double eulerians[] = {0., 90 * HKL_DEGTORAD, 0.};
	HklParameterList *pseudo_axes;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "eulerians");
	modes = hkl_engine_modes(engine);
	pseudo_axes = hkl_engine_pseudo_axes(engine);

	darray_foreach(mode, *modes){
		hkl_engine_select_mode(engine, *mode);
		if(darray_size((*mode)->parameters))
			hkl_parameter_set_value(darray_item((*mode)->parameters, 0), 1, NULL);

		/* studdy this degenerated case */
		hkl_parameter_list_set_values(pseudo_axes,
					      eulerians, ARRAY_SIZE(eulerians),
					      NULL);
		if (hkl_engine_set(engine, NULL)) {
			HklGeometryListItem *item;

			res &= geometries->len == 2;

			/* first solution = 0, 90, 0 */
			item = list_tail(&geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes_v(engine, 0., 90 * HKL_DEGTORAD, 0.);

			/* second solution = -180, -90, 180 */
			item = list_top(&geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes_v(engine, -180. * HKL_DEGTORAD, -90 * HKL_DEGTORAD, 180. * HKL_DEGTORAD);
		}
	}

	ok(res == HKL_TRUE, "eulerians");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void q(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "q");
	modes = hkl_engine_modes(engine);

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);

	darray_foreach(mode, *modes){
		double q;

		hkl_engine_select_mode(engine, *mode);
		for(q=-1.; q<1.; q += 0.1){
			hkl_engine_set_values_v(engine, q, NULL);
			if(hkl_engine_set(engine, NULL)){
				HklGeometryListItem *item;

				list_for_each(&geometries->items, item, node){
					hkl_engine_set_values_v(engine, 0.);
					hkl_geometry_init_geometry(geom, item->geometry);
					hkl_engine_get(engine, NULL);
					res &= check_pseudoaxes_v(engine, q);
				}
			}
		}
	}

	ok(res == HKL_TRUE, "q");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(3);

	degenerated();
	eulerians();
	q();

	return 0;
}
