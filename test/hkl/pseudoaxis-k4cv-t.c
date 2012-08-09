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

static void new(void)
{
	int res = 0;
	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_k4cv_hkl_new();
	hkl_pseudo_axis_engine_free(engine);
	ok(res == 0, "new");
}

static void degenerated(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i;
	double *H, *K, *L;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	H = &(((HklParameter *)engine->pseudoAxes[0])->value);
	K = &(((HklParameter *)engine->pseudoAxes[1])->value);
	L = &(((HklParameter *)engine->pseudoAxes[2])->value);

	list_for_each(&engine->modes, mode, list) {
		double h, k, l;
		int res;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		if(mode->parameters_len)
			engine->mode->parameters[0].value = 1.;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 1;
		*L = l = 0;

		if (hkl_pseudo_axis_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&engines->geometries->items, item, node) {
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(geom, item->geometry);
				hkl_pseudo_axis_engine_get(engine, NULL);
				res &= check_pseudoaxes(engine, h, k, l);
			}
		}
	}

	ok(res == HKL_TRUE, "degenerated");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void eulerians(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i;
	double *Omega, *Chi, *Phi;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "eulerians");

	Omega = &(((HklParameter *)engine->pseudoAxes[0])->value);
	Chi   = &(((HklParameter *)engine->pseudoAxes[1])->value);
	Phi   = &(((HklParameter *)engine->pseudoAxes[2])->value);

	list_for_each(&engine->modes, mode, list){
		double omega, chi, phi;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		if(mode->parameters_len)
			mode->parameters[0].value = 1.;

		/* studdy this degenerated case */
		*Omega = omega = 0;
		*Chi = chi = 90 * HKL_DEGTORAD;
		*Phi = phi = 0;

		if (hkl_pseudo_axis_engine_set(engine, NULL)) {
			HklGeometryListItem *item;

			res &= engines->geometries->len == 2;

			/* first solution = 0, 90, 0 */
			item = list_tail(&engines->geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			res &= check_pseudoaxes(engine, 0., 90 * HKL_DEGTORAD, 0.);

			/* second solution = -180, -90, 180 */
			item = list_top(&engines->geometries->items, HklGeometryListItem, node);
			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			res &= check_pseudoaxes(engine, -180. * HKL_DEGTORAD, -90 * HKL_DEGTORAD, 180. * HKL_DEGTORAD);
		}
	}

	ok(res == HKL_TRUE, "eulerians");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void q(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i;
	double *Q;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "q");

	Q = &(((HklParameter *)engine->pseudoAxes[0])->value);

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_pseudo_axis_engine_initialize(engine, NULL);

	list_for_each(&engine->modes, mode, list){
		double q;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		for(q=-1.; q<1.; q += 0.1){
			*Q = q;

			if(hkl_pseudo_axis_engine_set(engine, NULL)){
				HklGeometryListItem *item;

				list_for_each(&engines->geometries->items, item, node){
					*Q = 0;

					hkl_geometry_init_geometry(geom, item->geometry);
					hkl_pseudo_axis_engine_get(engine, NULL);
					res &= check_pseudoaxes(engine, q);
				}
			}
		}
	}

	ok(res == HKL_TRUE, "q");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(4);

	new();
	degenerated();
	eulerians();
	q();

	return 0;
}
