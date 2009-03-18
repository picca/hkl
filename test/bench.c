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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>
#include <sys/time.h>

#include <math.h>

#include <hkl/hkl-pseudoaxis-factory.h>

#define SET_AXES(geometry, mu, komega, kappa, kphi, gamma, delta) do{\
	hkl_geometry_set_values_v(geometry, 6,\
				  mu * HKL_DEGTORAD,\
				  komega * HKL_DEGTORAD,\
				  kappa * HKL_DEGTORAD,\
				  kphi * HKL_DEGTORAD,\
				  gamma * HKL_DEGTORAD,\
				  delta * HKL_DEGTORAD);\
} while(0)

static void hkl_test_bench()
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, j;
	double *H, *K, *L, h, k, l;
	int res, n;
	struct timeval debut, fin, dt;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_KAPPA6C);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

	H = &(((HklParameter *)engine->pseudoAxes[0])->value);
	K = &(((HklParameter *)engine->pseudoAxes[1])->value);
	L = &(((HklParameter *)engine->pseudoAxes[2])->value);

	/* studdy this degenerated case */
	*H = h = 1;
	*K = k = 0;
	*L = l = 0;

	// pseudo -> geometry
	n = 1000;
	for(j=0; j<HKL_LIST_LEN(engine->modes); ++j){
		hkl_pseudo_axis_engine_select_mode(engine, j);
		if (HKL_LIST_LEN(engine->mode->parameters))
			engine->mode->parameters[0].value = 1.;

		gettimeofday(&debut, NULL);
		for(i=0; i<n; ++i){
			SET_AXES(geom, 0, 0, 0, 0, 10, 10);
			res = hkl_pseudo_axis_engine_setter(engine, geom, &det, sample);
		}
		gettimeofday(&fin, NULL);
		timersub(&fin, &debut, &dt);
		fprintf(stdout, "%d %s (%d/%d) iterations %f ms each\n",
			j, engine->mode->name, n, i, (dt.tv_sec*1000.+dt.tv_usec/1000.)/n);
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

hkl_test_bench_eulerians()
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklGeometry *geom;
	HklDetector det = {1};
	HklSample *sample;
	size_t i, f_idx;
	double *Omega, *Chi, *Phi;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_KAPPA6C, 50 * HKL_DEGTORAD);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_KAPPA6C);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "eulerians");

	Omega = &(((HklParameter *)engine->pseudoAxes[0])->value);
	Chi   = &(((HklParameter *)engine->pseudoAxes[1])->value);
	Phi   = &(((HklParameter *)engine->pseudoAxes[2])->value);

	for(f_idx=0; f_idx<HKL_LIST_LEN(engine->modes); ++f_idx) {
		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		if (f_idx>0)
			engine->mode->parameters[0].value = 1.;

		double omega, chi, phi;
		int res;

		/* studdy this degenerated case */
		*Omega = omega = 0;
		*Chi = chi = 90 * HKL_DEGTORAD;
		*Phi = phi = 0;

		// pseudo -> geometry
		res = hkl_pseudo_axis_engine_setter(engine, geom, &det, sample);
		//hkl_pseudo_axis_engine_fprintf(stdout, engine);

		// geometry -> pseudo
		if (res == HKL_SUCCESS) {
			for(i=0; i<HKL_LIST_LEN(engines->geometries->geometries); ++i) {
				*Omega = *Chi = *Phi = 0;

				hkl_geometry_init_geometry(engine->geometry, engines->geometries->geometries[i]);
				hkl_pseudo_axis_engine_getter(engine, engine->geometry, &det, sample);
				//hkl_pseudo_axis_engine_fprintf(stdout, engine);
			}
		}
	}

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_geometry_free(geom);
}

int main(int argc, char **argv)
{
	size_t i;
	int res = 0;

	hkl_test_bench();

	return res;
}
