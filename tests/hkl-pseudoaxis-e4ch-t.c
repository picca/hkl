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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Jens Krüger <jens.krueger@frm.tum.de>
 */
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

static void getter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("E4CH");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl");

	/* geometry -> pseudo */
	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., 1., 0.);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 90., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., 0., 0.);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., -90., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, -1., 0., 0.);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 180., 60.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0., -1., 0.);

	hkl_geometry_set_values_unit_v(geometry, 45., 0., 135., 90.);
	hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 1., -1., 0.);

	ok(res == HKL_TRUE, "getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void degenerated(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklParameterList *pseudo_axes;

	factory = hkl_factory_get_by_name("E4CH");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries_get(engines);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl");
	modes = hkl_engine_modes_get(engine);
	pseudo_axes = hkl_engine_pseudo_axes_get(engine);

	darray_foreach(mode, *modes){
		static double values[] = {0, 0, 1};
		darray_parameter *parameters;

		hkl_engine_select_mode(engine, *mode);
		parameters = hkl_mode_parameters_get(*mode);
		if (darray_size(*parameters))
			hkl_parameter_value_set(darray_item(*parameters, 0), 0, NULL);

		/* studdy this degenerated case */
		hkl_parameter_list_values_set(pseudo_axes, values, 3, NULL);
		if(hkl_engine_set(engine, NULL)){
			const darray_item *items = hkl_geometry_list_items_get(geometries);
			HklGeometryListItem **item;

			darray_foreach(item, *items){
				static double null[] = {0, 0, 0};

				hkl_parameter_list_values_set(pseudo_axes, null, 3, NULL);
				hkl_geometry_set(geometry,
						 hkl_geometry_list_item_geometry_get(*item));
				hkl_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, values, 3);
			}
		}
	}

	ok(res == HKL_TRUE, "degenerated");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void psi_getter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	double hkl[3];
	HklMode *mode;
	HklParameterList *parameters;

	factory = hkl_factory_get_by_name("E4CH");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi");
	mode = hkl_engine_mode_get(engine);
	parameters = hkl_mode_parameters_get(mode);

	/* the getter part */
	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);

	hkl[0] = 1, hkl[1] = 0, hkl[2] = 0;
	hkl_parameter_list_values_set(parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 0.);

	/* here Q and <h, k, l>_ref are colinear must FAIL */
	hkl[0] = 0, hkl[1] = 1, hkl[2] = 0;
	hkl_parameter_list_values_set(parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= !hkl_engine_get(engine, NULL);

	hkl[0] = -1, hkl[1] = 0, hkl[2] = 0;
	hkl_parameter_list_values_set(parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 180. * HKL_DEGTORAD);

	hkl[0] = 0, hkl[1] = 0, hkl[2] = -1;
	hkl_parameter_list_values_set(parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= hkl_engine_get(engine, NULL);
	res &= check_pseudoaxes_v(engine, 90. * HKL_DEGTORAD);

	/* Q and <h, k, l>_ref are colinear so must FAIL */
	hkl[0] = 0, hkl[1] = -1, hkl[2] = 0;
	hkl_parameter_list_values_set(parameters,
				      hkl, ARRAY_SIZE(hkl), NULL);
	res &= !hkl_engine_get(engine, NULL);

	ok(res == HKL_TRUE, "psi getter");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
}

static void psi_setter(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 0};
	HklParameterList *pseudo_axes;

	factory = hkl_factory_get_by_name("E4CH");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries_get(engines);

	engine = hkl_engine_list_engine_get_by_name(engines, "psi");
	modes = hkl_engine_modes_get(engine);
	pseudo_axes = hkl_engine_pseudo_axes_get(engine);

	/* the init part */
	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(hkl_engine_mode_get(engine)),
				      hkl, ARRAY_SIZE(hkl), NULL);
	hkl_engine_initialize(engine, NULL);

	darray_foreach(mode, *modes){
		double psi;

		hkl_engine_select_mode(engine, *mode);
		for(psi=-180 * HKL_DEGTORAD;psi<180 * HKL_DEGTORAD;psi += HKL_DEGTORAD){
			hkl_parameter_list_values_set(pseudo_axes, &psi, 1, NULL);
			if(hkl_engine_set(engine, NULL)){
				const darray_item *items = hkl_geometry_list_items_get(geometries);
				HklGeometryListItem **item;

				darray_foreach(item, *items){
					static double null[] = {0};

					hkl_parameter_list_values_set(pseudo_axes,
								      null, ARRAY_SIZE(null),
								      NULL);
					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(*item));
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
	hkl_geometry_free(geometry);
}


static void q(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode **mode;
	darray_mode *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklParameterList *pseudo_axes;

	factory = hkl_factory_get_by_name("E4CH");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries_get(engines);

	engine = hkl_engine_list_engine_get_by_name(engines, "q");
	modes = hkl_engine_modes_get(engine);
	pseudo_axes = hkl_engine_pseudo_axes_get(engine);

	/* the init part */
	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);

	darray_foreach(mode, *modes){
		double q;

		hkl_engine_select_mode(engine, *mode);
		for(q=-1.; q<1.; q += 0.1){
			hkl_parameter_list_values_set(pseudo_axes, &q, 1, NULL);

			if(hkl_engine_set(engine, NULL)){
				const darray_item *items = hkl_geometry_list_items_get(geometries);
				HklGeometryListItem **item;

				darray_foreach(item, *items){
					static double null[] = {0};
					hkl_parameter_list_values_set(pseudo_axes,
								      null, ARRAY_SIZE(null),
								      NULL);

					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(*item));
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
	hkl_geometry_free(geometry);
}

static void hkl_psi_constant_horizontal(void)
{
	int res = HKL_TRUE;
	HklEngineList *engines;
	HklEngine *engine;
	HklMode *mode;
	const HklFactory *factory;
	HklGeometry *geometry;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	static double hkl[] = {1, 0, 1};
	static double hkl2[] = {1, 1, 0};
	HklParameterList *pseudo_axes;

	factory = hkl_factory_get_by_name("E4CH");
	geometry = hkl_factory_create_new_geometry(factory);
	sample = hkl_sample_new("test");

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);
	geometries = hkl_engine_list_geometries_get(engines);

	engine = hkl_engine_list_engine_get_by_name(engines, "hkl");
	mode = hkl_engine_mode_get(engine);
	pseudo_axes = hkl_engine_pseudo_axes_get(engine);

	hkl_engine_select_mode_by_name(engine,
				       "psi_constant");

	/* the init part */
	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	hkl_parameter_list_values_set(hkl_mode_parameters_get(mode),
				      hkl2, ARRAY_SIZE(hkl2), NULL);
	hkl_engine_initialize(engine, NULL);

	hkl_parameter_list_values_set(pseudo_axes,
				      hkl, ARRAY_SIZE(hkl), NULL);
	if(hkl_engine_set(engine, NULL)){
		const darray_item *items = hkl_geometry_list_items_get(geometries);
		HklGeometryListItem **item;

		darray_foreach(item, *items){
			static double null[] = {0, 0, 0};

			hkl_parameter_list_values_set(pseudo_axes,
						      null, ARRAY_SIZE(null),
						      NULL);
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(*item));
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes(engine, hkl, ARRAY_SIZE(hkl));
		}
	}

	ok(res == HKL_TRUE, "psi constant horizontal");

	hkl_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geometry);
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
