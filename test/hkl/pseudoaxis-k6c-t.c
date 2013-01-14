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

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
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

	darray_foreach(mode, *modes){
		HklParameterList *parameters;

		hkl_engine_select_mode(engine, *mode);
		parameters = hkl_mode_parameters(*mode);
		if (!strcasecmp(hkl_mode_name(*mode),
				"constant_chi_vertical"))
			hkl_parameter_set_value(darray_item(*parameters, 0), 1, NULL);
		if (!strcasecmp(hkl_mode_name(*mode),
				"constant_incidence"))
			hkl_parameter_set_value(darray_item(*parameters, 3), 1, NULL);

		/* studdy this degenerated case */
		hkl_parameter_list_set_values(pseudo_axes,
					      hkl, ARRAY_SIZE(hkl), NULL);
		if (hkl_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&geometries->items, item, node) {
				static double null[] = {0, 0, 0};

				hkl_parameter_list_set_values(pseudo_axes,
							      null, ARRAY_SIZE(null),
							      NULL);
				hkl_geometry_init_geometry(geom, item->geometry);
				hkl_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, hkl, ARRAY_SIZE(hkl));
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
	HklGeometryListItem *item;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "eulerians");
	modes = hkl_engine_modes(engine);

	darray_foreach(mode, *modes){
		double omega, chi, phi;
		HklParameterList *parameters; 

		hkl_engine_select_mode(engine, *mode);
		parameters = hkl_mode_parameters(*mode);
		if (darray_size(*parameters))
			hkl_parameter_set_value(darray_item(*parameters, 0), 1, NULL);

		/* studdy this degenerated case */
		hkl_engine_set_values_v(engine, 0., 90. * HKL_DEGTORAD, 0.);
		if (hkl_engine_set(engine, NULL)) {
			res &= geometries->len == 2;

			/* first solution = 0, 90, 0 */
			item = list_tail(&geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_engine_get(engine, NULL);
			res &= check_pseudoaxes_v(engine, 0., 90. * HKL_DEGTORAD, 0.);

			/* second solution = -180, -90, 180 */
			item = list_top(&geometries->items, HklGeometryListItem, node);
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
	HklMode **mode;
	darray_mode *modes;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	const HklGeometryList *geometries;
	HklDetector *detector;
	HklSample *sample;
	HklParameterList *pseudo_axes;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geom, detector, sample);
	geometries = hkl_engine_list_geometries(engines);

	engine = hkl_engine_list_get_by_name(engines, "q2");
	modes = hkl_engine_modes(engine);
	pseudo_axes = hkl_engine_pseudo_axes(engine);

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 0., 0., 60.);
	hkl_engine_initialize(engine, NULL);

	darray_foreach(mode, *modes){
		double q, alpha;

		hkl_engine_select_mode(engine, *mode);
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				double values[] = {q, alpha};

				hkl_parameter_list_set_values(pseudo_axes,
							      values, ARRAY_SIZE(values),
							      NULL);
				if(hkl_engine_set(engine, NULL)){
					HklGeometryListItem *item;

					list_for_each(&geometries->items, item, node){
						static double null[] = {0, 0};

						hkl_parameter_list_set_values(pseudo_axes,
									      null, ARRAY_SIZE(null),
									      NULL);
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
