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
 *          Jens Krüger <jens.krueger@frm.tum.de>
 */
#include <hkl.h>
#include <tap/basic.h>
#include <tap/hkl.h>

static void getter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "hkl");

	/* geometry -> pseudo */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., 1., 0.);

	hkl_geometry_set_values_unit_v(geom, 30., 0., 90., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., 0., 0.);

	hkl_geometry_set_values_unit_v(geom, 30., 0., -90., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, -1., 0., 0.);

	hkl_geometry_set_values_unit_v(geom, 30., 0., 180., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., -1., 0.);

	hkl_geometry_set_values_unit_v(geom, 45., 0., 135., 90.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., -1., 0.);

	ok(res == HKL_TRUE, "getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

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

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "hkl");

	list_for_each(&engine->modes, mode, list){
		static double values[] = {0, 0, 1};

		hkl_engine_select_mode(engine, mode);
		if (darray_size(engine->mode->parameters)){
			static double zero[] = {0};
			hkl_parameter_list_set_values(&engine->mode->parameters,
						      zero, 1, NULL);
		}

		/* studdy this degenerated case */
		hkl_parameter_list_set_values(&engine->pseudo_axes, values, 3, NULL);
		if(hkl_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&engines->geometries->items, item, node){
				static double null[] = {0, 0, 0};

				hkl_parameter_list_set_values(&engine->pseudo_axes, null, 3, NULL);
				hkl_geometry_init_geometry(engines->geometry, item->geometry);
				hkl_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, values, 3);
			}
		}
	}

	ok(res == HKL_TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void psi_getter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	double hkl[3];

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "psi");

	/* the getter part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);

	hkl[0] = 1, hkl[1] = 0, hkl[2] = 0;
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0.);

	/* here Q and <h, k, l>_ref are colinear must FAIL */
	hkl[0] = 0, hkl[1] = 1, hkl[2] = 0;
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= !hkl_engine_get(engine, NULL);

	hkl[0] = -1, hkl[1] = 0, hkl[2] = 0;
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 180. * HKL_DEGTORAD);

	hkl[0] = 0, hkl[1] = 0, hkl[2] = -1;
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 90. * HKL_DEGTORAD);

	/* Q and <h, k, l>_ref are colinear so must FAIL */
	hkl[0] = 0, hkl[1] = -1, hkl[2] = 0;
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= !hkl_engine_get(engine, NULL);

	ok(res == HKL_TRUE, "psi getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void psi_setter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 0};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "psi");

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	hkl_engine_initialize(engine, NULL);

	list_for_each(&engine->modes, mode, list){
		double psi;

		hkl_engine_select_mode(engine, mode);
		for(psi=-180 * HKL_DEGTORAD;psi<180 * HKL_DEGTORAD;psi += HKL_DEGTORAD){
			hkl_parameter_list_set_values(&engine->pseudo_axes, &psi, 1, NULL);
			if(hkl_engine_set(engine, NULL)){
				HklGeometryListItem *item;

				list_for_each(&engines->geometries->items, item, node){
					static double null[] = {0};

					hkl_parameter_list_set_values(&engine->pseudo_axes, null, 1, NULL);
					hkl_geometry_init_geometry(geom, item->geometry);
					hkl_engine_get(engine, NULL);
					res &= check_pseudoaxes_v(engine, psi);
				}
			}
		}
	}

	ok(res == HKL_TRUE, "psi setter");

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
	HklMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "q");

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);

	list_for_each(&engine->modes, mode, list){
		double q;

		hkl_engine_select_mode(engine, mode);
		for(q=-1.; q<1.; q += 0.1){
			hkl_parameter_list_set_values(&engine->pseudo_axes, &q, 1, NULL);

			if(hkl_engine_set(engine, NULL)){
				HklGeometryListItem *item;

				list_for_each(&engines->geometries->items, item, node){
					static double null[] = {0};
					hkl_parameter_list_set_values(&engine->pseudo_axes, null, 1, NULL);

					hkl_geometry_init_geometry(geom, item->geometry);
					hkl_engine_get(engine, NULL);
					res &= check_pseudoaxes(engine, &q, 1);
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

static void hkl_psi_constant_horizontal(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 1};
	static double hkl2[] = {1, 1, 0};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "hkl");

	hkl_engine_select_mode_by_name(engine,
						   "psi_constant");

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_parameter_list_set_values(&engine->mode->parameters,
				      hkl2, ARRAY_SIZE(hkl2), NULL);
	hkl_engine_initialize(engine, NULL);

	hkl_parameter_list_set_values(&engine->pseudo_axes,
				      hkl, ARRAY_SIZE(hkl), NULL);
	if(hkl_engine_set(engine, NULL)){
		HklGeometryListItem *item;

		list_for_each(&engines->geometries->items, item, node){
			static double null[] = {0, 0, 0};

			hkl_parameter_list_set_values(&engine->pseudo_axes,
						      null, ARRAY_SIZE(null),
						      NULL);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes(engine, hkl, ARRAY_SIZE(hkl));
		}
	}

	ok(res == HKL_TRUE, "psi constant horizontal");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(6);

	getter();
	degenerated();
	psi_getter();
	psi_setter();
	q();
	hkl_psi_constant_horizontal();

	return 0;
}
