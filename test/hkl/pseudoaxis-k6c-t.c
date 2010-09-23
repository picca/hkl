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
	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_k6c_hkl_new();
	hkl_pseudo_axis_engine_free(engine);
	ok(res == 0, "new");
}

static void degenerated(void)
{
	int res = 0;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *H, *K, *L;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
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

	for(f_idx=0; f_idx<engine->modes_len; ++f_idx) {
		double h, k, l;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		if (engine->mode->parameters_len)
			engine->mode->parameters[0].value = 1.;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 1;
		*L = l = 0;

		if (hkl_pseudo_axis_engine_set(engine, NULL) == HKL_SUCCESS)
			for(i=0; i<engines->geometries->len; ++i) {
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(geom,
							   engines->geometries->items[i].geometry);
				hkl_pseudo_axis_engine_get(engine, NULL);
				res |= check_pseudoaxes(engine, h, k, l);
			}
	}

	ok(res == 0, "degenerated");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void eulerians(void)
{
	int res = 0;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *Omega, *Chi, *Phi;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
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

	for(f_idx=0; f_idx<engine->modes_len; ++f_idx) {
		double omega, chi, phi;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		if (f_idx>0)
			engine->mode->parameters[0].value = 1.;

		/* studdy this degenerated case */
		*Omega = omega = 0;
		*Chi = chi = 90 * HKL_DEGTORAD;
		*Phi = phi = 0;

		if (hkl_pseudo_axis_engine_set(engine, NULL) == HKL_SUCCESS) {
			res |= !(2 == engines->geometries->len);

			/* first solution = 0, 90, 0 */
			hkl_geometry_init_geometry(geom,
						   engines->geometries->items[1].geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			check_pseudoaxes(engine, 0., 90. * HKL_DEGTORAD, 0.); 

			/* second solution = -180, -90, 180 */
			hkl_geometry_init_geometry(geom,
						   engines->geometries->items[0].geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			check_pseudoaxes(engine, -180. * HKL_DEGTORAD, -90. * HKL_DEGTORAD, 180. * HKL_DEGTORAD); 
		}
	}

	ok(res == 0, "eulerians");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void q2(void)
{
	int res = 0;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *Q, *Alpha;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "q2");

	Q = &(((HklParameter *)engine->pseudoAxes[0])->value);
	Alpha = &(((HklParameter *)engine->pseudoAxes[1])->value);

	/* the init part */
	hkl_geometry_set_values_unit_v(geom, 0., 30., 0., 0., 0., 60.);
	hkl_pseudo_axis_engine_initialize(engine, NULL);


	for(f_idx=0; f_idx<engine->modes_len; ++f_idx){
		double q, alpha;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		for(q=0.1; q<1.; q += 0.1)
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				*Q = q;
				*Alpha = alpha;
			
				if(hkl_pseudo_axis_engine_set(engine, NULL) == HKL_SUCCESS)
					for(i=0; i<engines->geometries->len; ++i){
						*Q = 0;
						*Alpha = 0.;
					
						hkl_geometry_init_geometry(geom,
									   engines->geometries->items[i].geometry);
						hkl_pseudo_axis_engine_get(engine, NULL);
						res |= check_pseudoaxes(engine, q, alpha);
					}
			}
	}

	ok(res == 0, "q2");

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}


static void m15110(void)
{
	int res = 0;
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	/* the init part must succed */
	hkl_geometry_set_values_unit_v(geom, 0., 62.95, 134.75, 0., 0., 60.);
	res |= hkl_pseudo_axis_engine_initialize(engine, NULL);

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	ok(res == 0, "m15110");
}

int main(int argc, char** argv)
{
	plan(5);

	new();
	degenerated();
	eulerians();
	q2();
	m15110(); 

	return 0;
}
