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
 * Copyright (C) 2003-2012 Synchrotron SOLEIL
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
	HklMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {0, 1, 0};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "hkl");

	list_for_each(&engine->modes, mode, list){
		hkl_engine_select_mode(engine, mode);
		if (mode->parameters.len){
			static double one[] = {1.};

			hkl_parameter_list_set_values(&engine->mode->parameters,
						      one, 1, NULL);
		}

		/* studdy this degenerated case */
		hkl_parameter_list_set_values(&engine->pseudo_axes, hkl, 3, NULL);
		if (hkl_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&engines->geometries->items, item, node) {
				static double null[] = {0, 0, 0};

				hkl_parameter_list_set_values(&engine->pseudo_axes, null, 3, NULL);
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
	HklMode *mode;
	HklGeometryListItem *item;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "eulerians");

	list_for_each(&engine->modes, mode, list){
		double omega, chi, phi;

		hkl_engine_select_mode(engine, mode);
		if (mode->parameters.len){
			static double one[] = {1};

			hkl_parameter_list_set_values(&engine->mode->parameters,
						      one, 1, NULL);
		}

		/* studdy this degenerated case */
		hkl_engine_set_values_v(engine, 0., 90. * HKL_DEGTORAD, 0.);
		if (hkl_engine_set(engine, NULL)) {
			res &= engines->geometries->len == 2;

			/* first solution = 0, 90, 0 */
			item = list_tail(&engines->geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes_v(engine, 0., 90. * HKL_DEGTORAD, 0.);

			/* second solution = -180, -90, 180 */
			item = list_top(&engines->geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes_v(engine, -180. * HKL_DEGTORAD, -90. * HKL_DEGTORAD, 180. * HKL_DEGTORAD);
		}
	}

	ok(res == HKL_TRUE, "eulerians");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void q2(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "q2");

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);


	list_for_each(&engine->modes, mode, list){
		double q, alpha;

		hkl_engine_select_mode(engine, mode);
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				double values[] = {q, alpha};

				hkl_parameter_list_set_values(&engine->pseudo_axes, values, 2, NULL);
				if(hkl_engine_set(engine, NULL)){
					HklGeometryListItem *item;

					list_for_each(&engines->geometries->items, item, node){
						static double null[] = {0, 0};

						hkl_parameter_list_set_values(&engine->pseudo_axes, null, 2, NULL);
						hkl_geometry_init_geometry(geom, item->geometry);
						hkl_engine_get(engine, NULL);
						res &= check_pseudoaxes(engine, values, 2);
					}
				}
			}
	}

	ok(res == HKL_TRUE, "q2");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}


static void m15110(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "psi");

	/* the init part must succed */
	hkl_geometry_set_values_unit_v(geom, 0., 62.95, 134.75, 0., 0., 60.);
	res &= hkl_engine_initialize(engine, NULL);

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	ok(res == HKL_TRUE, "m15110");
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
