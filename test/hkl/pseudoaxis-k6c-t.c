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

#define SET_AXES(geometry, mu, komega, kappa, kphi, gamma, delta) do{	\
		hkl_geometry_set_values_v(geometry, 6,			\
					  mu * HKL_DEGTORAD,		\
					  komega * HKL_DEGTORAD,	\
					  kappa * HKL_DEGTORAD,		\
					  kphi * HKL_DEGTORAD,		\
					  gamma * HKL_DEGTORAD,		\
					  delta * HKL_DEGTORAD);	\
	} while(0)

static void new(void)
{
	HklPseudoAxisEngine *engine = hkl_pseudo_axis_engine_k6c_hkl_new();
	hkl_pseudo_axis_engine_free(engine);
}

static void degenerated(void)
{
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
		int res;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		if (engine->mode->parameters_len)
			engine->mode->parameters[0].value = 1.;

		/* studdy this degenerated case */
		*H = h = 0;
		*K = k = 1;
		*L = l = 0;

		/* pseudo -> geometry */
		res = hkl_pseudo_axis_engine_set(engine, NULL);
		/* hkl_pseudo_axis_engine_fprintf(stdout, engine); */

		/* geometry -> pseudo */
		if (res == HKL_SUCCESS) {
			/* hkl_pseudo_axis_engine_fprintf(stdout, engine); */
			for(i=0; i<hkl_geometry_list_len(engines->geometries); ++i) {
				*H = *K = *L = 0;

				hkl_geometry_init_geometry(geom,
							   engines->geometries->items[i].geometry);
				hkl_pseudo_axis_engine_get(engine, NULL);

				is_double(h, *H, HKL_EPSILON, __func__);
				is_double(k, *K, HKL_EPSILON, __func__);
				is_double(l, *L, HKL_EPSILON, __func__);
			}
		}
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void eulerians(void)
{
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
		int res;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		if (f_idx>0)
			engine->mode->parameters[0].value = 1.;

		/* studdy this degenerated case */
		*Omega = omega = 0;
		*Chi = chi = 90 * HKL_DEGTORAD;
		*Phi = phi = 0;

		/* pseudo -> geometry */
		res = hkl_pseudo_axis_engine_set(engine, NULL);
		/* hkl_pseudo_axis_engine_fprintf(stdout, engine); */

		/* geometry -> pseudo */
		if (res == HKL_SUCCESS) {
			is_int(2, hkl_geometry_list_len(engines->geometries), __func__);

			/* first solution = 0, 90, 0 */
			hkl_geometry_init_geometry(geom,
						   engines->geometries->items[1].geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			is_double(0., *Omega, HKL_EPSILON, __func__);
			is_double(90. * HKL_DEGTORAD, *Chi, HKL_EPSILON, __func__);
			is_double(0. * HKL_DEGTORAD, *Phi, HKL_EPSILON, __func__);

			hkl_geometry_init_geometry(geom,
						   engines->geometries->items[0].geometry);
			hkl_pseudo_axis_engine_get(engine, NULL);
			is_double(-180.* HKL_DEGTORAD, *Omega, HKL_EPSILON, __func__);
			is_double(-90. * HKL_DEGTORAD, *Chi, HKL_EPSILON, __func__);
			is_double(180. * HKL_DEGTORAD, *Phi, HKL_EPSILON, __func__);
		}
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void manip(void)
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *hkl;
	HklPseudoAxisEngine *psi;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, f_idx;
	double *H, *K, *L;
	double H2, K2, L2;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	hkl_source_init(&geom->source, 2.0837, 1., 0., 0.);
	hkl_source_fprintf(stdout, &geom->source);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	/* hkl_matrix_init_from_euler(&sample->U, -90 * HKL_DEGTORAD, 0., 0.); */
	hkl_sample_set_lattice(sample,
			       2.88, 2.88, 2.88,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
	hkl_matrix_init_from_euler(&sample->U, -90 * HKL_DEGTORAD, 0., 0.);
	hkl_sample_fprintf(stdout, sample);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	hkl = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");
	psi = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	H = &(((HklParameter *)hkl->pseudoAxes[0])->value);
	K = &(((HklParameter *)hkl->pseudoAxes[1])->value);
	L = &(((HklParameter *)hkl->pseudoAxes[2])->value);

	hkl_pseudo_axis_engine_select_mode(hkl, 9);

	*H = 0;
	*K = 0;
	*L = 1. - 0.047;
	H2 = hkl->mode->parameters[0].value = 0;
	K2 = hkl->mode->parameters[1].value = 1.;
	L2 = hkl->mode->parameters[2].value = 1. - 2*0.047;
	if( HKL_SUCCESS == hkl_pseudo_axis_engine_set(hkl, NULL)){
		for(i=0; i<hkl_geometry_list_len(engines->geometries); ++i) {
			*H = *K = *L = 0;

			hkl_geometry_init_geometry(geom,
						   engines->geometries->items[i].geometry);
			hkl_pseudo_axis_engine_initialize(psi, NULL);
			hkl_pseudo_axis_engine_list_get(engines);
			hkl_pseudo_axis_engine_list_fprintf(stdout, engines);
		}
	}

	hkl_geometry_init_geometry(geom,
				   engines->geometries->items[0].geometry);
	hkl_pseudo_axis_engine_select_mode(hkl, 7);
	*H = H2;
	*K = K2;
	*L = L2;
	fprintf(stdout, "coucou\n");
	if (HKL_SUCCESS == hkl_pseudo_axis_engine_set(hkl, NULL)){
		fprintf(stdout, "coucou\n");
		for(i=0; i<hkl_geometry_list_len(engines->geometries); ++i){
			*H = *K = *L = 0;
			hkl_geometry_init_geometry(geom,
						   engines->geometries->items[i].geometry);
			hkl_pseudo_axis_engine_list_get(engines);
			hkl_pseudo_axis_engine_list_fprintf(stdout, engines);
		}
	}
	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

static void q2(void)
{
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
	SET_AXES(geom, 0., 30., 0., 0., 0., 60.);
	hkl_pseudo_axis_engine_initialize(engine, NULL);


	for(f_idx=0; f_idx<engine->modes_len; ++f_idx){
		double q, alpha;
		int res;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		for(q=0.1; q<1.; q += 0.1){
			for(alpha = -M_PI; alpha<M_PI; alpha += M_PI/180.){
				*Q = q;
				*Alpha = alpha;
			
				/* pseudo -> geometry */
				res = hkl_pseudo_axis_engine_set(engine, NULL);
			
				/* geometry -> pseudo */
				if(res == HKL_SUCCESS){
					for(i=0; i<hkl_geometry_list_len(engines->geometries); ++i){
						*Q = 0;
						*Alpha = 0.;
					
						hkl_geometry_init_geometry(geom,
									   engines->geometries->items[i].geometry);
						hkl_pseudo_axis_engine_get(engine, NULL);

						/* why this precision problem ?	*/
						is_double(q, *Q, HKL_EPSILON * 10, __func__);
						is_double(alpha, *Alpha, HKL_EPSILON, __func__);
					}
				}
			}
		}
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}


static void m15110(void)
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	int res;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "psi");

	/* the init part must succed */
	SET_AXES(geom, 0., 62.95, 134.75, 0., 0., 60.);
	res = hkl_pseudo_axis_engine_initialize(engine, NULL);

	ok(HKL_SUCCESS == res, __func__);

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char** argv)
{
	plan(28814);

	new();
	degenerated();
	eulerians();
	q2();
	m15110(); 

	return 0;
}