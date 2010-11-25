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
#include <stdio.h>
#include <sys/time.h>
#include <hkl.h>

static void hkl_test_bench_run(HklPseudoAxisEngine *engine, HklGeometry *geometry, size_t n)
{
	size_t i, j;
	struct timeval debut, fin, dt;

	/* pseudo -> geometry */
	for(j=0; j<engine->modes_len; ++j){
		hkl_pseudo_axis_engine_select_mode(engine, j);
		if (engine->mode->parameters_len)
			engine->mode->parameters[0].value = 1.;

		gettimeofday(&debut, NULL);
		for(i=0; i<n; ++i){
			hkl_geometry_set_values_unit_v(geometry, 0, 0, 0, 0, 10, 10);
			hkl_pseudo_axis_engine_set(engine, NULL);
		}
		gettimeofday(&fin, NULL);
		timersub(&fin, &debut, &dt);
		fprintf(stdout, "%d %s (%d/%d) iterations %f ms each\n",
			j, engine->mode->name, n, i, (dt.tv_sec*1000.+dt.tv_usec/1000.)/n);
	}

}

static void hkl_test_bench_hkl_real(HklPseudoAxisEngineList *engines, HklGeometry *geometry,
				    char const *name, int n,
				    double h, double k, double l)
{
	HklPseudoAxisEngine *engine;

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, name);

	((HklParameter *)engine->pseudoAxes[0])->value = h;
	((HklParameter *)engine->pseudoAxes[1])->value = k;
	((HklParameter *)engine->pseudoAxes[2])->value = l;

	hkl_test_bench_run(engine, geometry, n);
}

static void hkl_test_bench_eulerians_real(HklPseudoAxisEngineList *engines, HklGeometry *geometry,
					  char const *name, int n,
					  double omega, double chi, double phi)
{
	HklPseudoAxisEngine *engine;

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, name);

	((HklParameter *)engine->pseudoAxes[0])->value = omega;
	((HklParameter *)engine->pseudoAxes[1])->value = chi;
	((HklParameter *)engine->pseudoAxes[2])->value = phi;

	hkl_test_bench_run(engine, geometry, n);
}

static void hkl_test_bench_psi_real(HklPseudoAxisEngineList *engines, HklGeometry *geometry,
				    char const *name, int n,
				    double psi)
{
	HklPseudoAxisEngine *engine;

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, name);

	((HklParameter *)engine->pseudoAxes[0])->value = psi;

	hkl_test_bench_run(engine, geometry, n);
}

static void hkl_test_bench_q2_real(HklPseudoAxisEngineList *engines, HklGeometry *geometry,
				   char const *name, int n,
				   double q, double alpha)
{
	HklPseudoAxisEngine *engine;

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, name);

	((HklParameter *)engine->pseudoAxes[0])->value = q;
	((HklParameter *)engine->pseudoAxes[1])->value = alpha;

	hkl_test_bench_run(engine, geometry, n);
}

static void hkl_test_bench_k6c(int n)
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, j;
	int res;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	hkl_test_bench_hkl_real(engines, geom, "hkl", n, 1, 0, 0 );
	hkl_test_bench_eulerians_real(engines, geom, "eulerians", n, 0, 90*HKL_DEGTORAD, 0 );
	hkl_test_bench_psi_real(engines, geom, "psi", n, 10*HKL_DEGTORAD);
	hkl_test_bench_q2_real(engines, geom, "q2", n, 1, 10*HKL_DEGTORAD);

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void hkl_test_bench_eulerians(void)
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
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
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
			for(i=0; i<engines->geometries->len; ++i) {
				*Omega = *Chi = *Phi = 0;

				hkl_geometry_init_geometry(engine->geometry,
							   engines->geometries->items[i].geometry);
				hkl_pseudo_axis_engine_get(engine, NULL);
				/* hkl_pseudo_axis_engine_fprintf(stdout, engine); */
			}
		}
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

int main(int argc, char **argv)
{
	int n;

	plan(1);

	if (argc > 1)
		n = atoi(argv[1]);
	else
		n = 10;

	hkl_test_bench_k6c(n);
	hkl_test_bench_eulerians();

	ok(HKL_TRUE == HKL_TRUE, __func__);

	return 0;
}
