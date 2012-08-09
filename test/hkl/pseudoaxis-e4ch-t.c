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

static void new(void)
{
	int res = 0;

	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_e4c_hkl_new();
	hkl_pseudo_axis_engine_free(engine);

	ok(res == 0, "new");
}

static void getter(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	/* geometry -> pseudo */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 0., 1., 0.);

	hkl_geometry_set_values_unit_v(geom, 30., 0., 90., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 1., 0., 0.);

	hkl_geometry_set_values_unit_v(geom, 30., 0., -90., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, -1., 0., 0.);

	hkl_geometry_set_values_unit_v(geom, 30., 0., 180., 60.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 0., -1., 0.);

	hkl_geometry_set_values_unit_v(geom, 45., 0., 135., 90.);
	hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 1., -1., 0.);

	ok(res == HKL_TRUE, "getter");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
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

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	H = &(((HklParameter *)engine->pseudoAxes[0])->value);
	K = &(((HklParameter *)engine->pseudoAxes[1])->value);
	L = &(((HklParameter *)engine->pseudoAxes[2])->value);

	list_for_each(&engine->modes, mode, list){
		double h, k, l;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		if (engine->mode->parameters_len)
			engine->mode->parameters[0].value = 0.;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 0;
		*L = l = 1;

		if(hkl_pseudo_axis_engine_set(engine, NULL)){
			HklGeometryListItem *item;

			list_for_each(&engines->geometries->items, item, node){
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(engines->geometry, item->geometry);
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

static void psi_getter(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *psi;
	double *h_ref;
	double *k_ref;
	double *l_ref;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	psi = &(((HklParameter *)engine->pseudoAxes[0])->value);
	h_ref = &engine->mode->parameters[0].value;
	k_ref = &engine->mode->parameters[1].value;
	l_ref = &engine->mode->parameters[2].value;

	/* the getter part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	hkl_pseudo_axis_engine_initialize(engine, NULL);

	*h_ref = 1;
	*k_ref = 0;
	*l_ref = 0;
	res &= hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 0.);

	*h_ref = /* 0 */ 1;
	*k_ref = /* 1 */ 0;
	*l_ref = /* 0 */ 0;

	res &= hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 0. * HKL_DEGTORAD);

	/* here Q and <h, k, l>_ref are colinear must FAIL */
	*h_ref = 0;
	*k_ref = 1;
	*l_ref = 0;
	res &= !hkl_pseudo_axis_engine_get(engine, NULL);

	*h_ref = -1;
	*k_ref = 0;
	*l_ref = 0;
	res &= hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 180. * HKL_DEGTORAD);

	*h_ref = 0;
	*k_ref = 0;
	*l_ref = -1;
	res &= hkl_pseudo_axis_engine_get(engine, NULL);
	res &= check_pseudoaxes(engine, 90. * HKL_DEGTORAD);

	/* Q and <h, k, l>_ref are colinear so must FAIL */
	*h_ref = 0;
	*k_ref = -1;
	*l_ref = 0;
	res &= !hkl_pseudo_axis_engine_get(engine, NULL);

	ok(res == HKL_TRUE, "psi getter");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void psi_setter(void)
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
	double *Psi;
	double *h_ref, *k_ref, *l_ref;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	Psi = &(((HklParameter *)engine->pseudoAxes[0])->value);
	h_ref = &engine->mode->parameters[0].value;
	k_ref = &engine->mode->parameters[1].value;
	l_ref = &engine->mode->parameters[2].value;

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	*h_ref = 1;
	*k_ref = 0;
	*l_ref = 0;
	hkl_pseudo_axis_engine_initialize(engine, NULL);

	list_for_each(&engine->modes, mode, list){
		double psi;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		for(psi=-180;psi<180;psi++){
			*Psi = psi * HKL_DEGTORAD;

			if(hkl_pseudo_axis_engine_set(engine, NULL)){
				HklGeometryListItem *item;

				list_for_each(&engines->geometries->items, item, node){
					*Psi = 0;

					hkl_geometry_init_geometry(geom, item->geometry);
					hkl_pseudo_axis_engine_get(engine, NULL);
					res &= check_pseudoaxes(engine, psi * HKL_DEGTORAD);
				}
			}
		}
	}

	ok(res == HKL_TRUE, "psi setter");

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
	size_t i, f_idx;
	double *Q;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
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
					hkl_geometry_fprintf(stderr, geom);
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

static void hkl_psi_constant_horizontal(void)
{
	int res = HKL_TRUE;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *H, *K, *L;
	double h, k, l;
	double *h_ref, *k_ref, *l_ref, *psi_ref;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geom = hkl_geometry_factory_new(config);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	H = &(((HklParameter *)engine->pseudoAxes[0])->value);
	K = &(((HklParameter *)engine->pseudoAxes[1])->value);
	L = &(((HklParameter *)engine->pseudoAxes[2])->value);


	hkl_pseudo_axis_engine_select_mode_by_name(engine,
						   "psi_constant");
	h_ref = &engine->mode->parameters[0].value;
	k_ref = &engine->mode->parameters[1].value;
	l_ref = &engine->mode->parameters[2].value;
	psi_ref = &engine->mode->parameters[3].value;

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 30., 0., 0., 60.);
	*h_ref = 1;
	*k_ref = 1;
	*l_ref = 0;
	hkl_pseudo_axis_engine_initialize(engine, NULL);

	*H = h = 1;
	*K = k = 0;
	*L = l = 1;

	if(hkl_pseudo_axis_engine_set(engine, NULL)){
		HklGeometryListItem *item;

		list_for_each(&engines->geometries->items, item, node){
			*H = *K = *L = 0;

			hkl_geometry_init_geometry(geom, item->geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			res &= check_pseudoaxes(engine, h, k, l);
		}
	}

	ok(res == HKL_TRUE, "psi constant horizontal");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(7);

	new();
	getter();
	degenerated();
	psi_getter();
	psi_setter();
	q();
	hkl_psi_constant_horizontal();

	return 0;
}
