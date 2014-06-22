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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>
#include <sys/time.h>
#include <tap/basic.h>
#include "hkl.h"

static void hkl_test_bench_run_real(HklEngine *engine, HklGeometry *geometry, size_t n)
{
	size_t i;
	const darray_string *modes = hkl_engine_modes_names_get(engine);
	const char **mode;

	/* pseudo -> geometry */
	darray_foreach(mode, *modes){
		double min, max, mean;
		const darray_string *parameters;
		const char **parameter;

		hkl_engine_select_mode(engine, *mode, NULL);
		parameters = hkl_engine_parameters_names_get(engine);
		if (darray_size(*parameters)){
			HklParameter *p;
			const char *pname = darray_item(*parameters, 0);

			p = hkl_parameter_new_copy(hkl_engine_parameter_get(engine, pname, NULL));
			hkl_parameter_value_set(p, 1, HKL_UNIT_DEFAULT, NULL);
			hkl_engine_parameter_set(engine, pname, p, NULL);
			hkl_parameter_free(p);
		}

		mean = max = 0;
		min = 1000; /* arbitrary value always greater than the real min */
		for(i=0; i<n; ++i){
			struct timeval debut, fin, dt;
			double t;

			hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 0., 0., 0., 0., 10., 10.);
			gettimeofday(&debut, NULL);
			hkl_engine_set(engine, NULL);
			gettimeofday(&fin, NULL);
			timersub(&fin, &debut, &dt);
			t = dt.tv_sec * 1000. + dt.tv_usec / 1000.;
			min = t < min ? t : min;
			max = t > max ? t : max;
			mean += t;
		}
		fprintf(stdout, "\"%s\" \"%s\" \"%s\" (%d/%d) iterations %f / %f / %f [min/mean/max] ms each\n",
			hkl_geometry_name_get(geometry),
			hkl_engine_name_get(engine),
			*mode, n, i, min, mean/n, max);
	}
}

static void hkl_test_bench_run_v(HklEngineList *engines, HklGeometry *geometry,
				 char const *name, unsigned int n, ...)
{
	va_list ap;
	size_t i;
	HklEngine *engine = hkl_engine_list_engine_get_by_name(engines, name, NULL);
	size_t n_values = darray_size(*hkl_engine_pseudo_axes_names_get(engine));
	double values[n_values];

	va_start(ap, n);
	for(i=0; i<n_values; ++i)
		values[i] = va_arg(ap, double);
	va_end(ap);

	hkl_engine_pseudo_axes_values_set(engine, values, n_values, HKL_UNIT_DEFAULT, NULL);

	hkl_test_bench_run_real(engine, geometry, n);
}

static void hkl_test_bench_k6c(int n)
{
	const HklFactory *factory;
	HklEngineList *engines;
	HklEngine *engine;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;
	size_t i, j;
	int res;

	factory = hkl_factory_get_by_name("K6C", NULL);

	geom = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");

	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geom, detector, sample);

	hkl_test_bench_run_v(engines, geom, "hkl", n, 1., 0., 0.);
	hkl_test_bench_run_v(engines, geom, "eulerians", n, 0., 90*HKL_DEGTORAD, 0.);
	hkl_test_bench_run_v(engines, geom, "psi", n, 10.*HKL_DEGTORAD);
	hkl_test_bench_run_v(engines, geom, "q2", n, 1., 10.*HKL_DEGTORAD);
	hkl_test_bench_run_v(engines, geom, "qper_qpar", n, 1., 1.);

	hkl_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void hkl_test_bench_eulerians(void)
{
	HklEngineList *engines;
	HklEngine *engine;
	const char **mode;
	const darray_string *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("K6C", NULL);

	geometry = hkl_factory_create_new_geometry(factory);
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");
	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_engine_get_by_name(engines, "eulerians", NULL);
	modes = hkl_engine_modes_names_get(engine);

	darray_foreach(mode, *modes){
		static double eulerians[] = {0, 90 * HKL_DEGTORAD, 0};

		hkl_engine_select_mode(engine, *mode, NULL);

		/* studdy this degenerated case */
		hkl_engine_pseudo_axes_values_set(engine, eulerians, 3, HKL_UNIT_DEFAULT, NULL);
		if (hkl_engine_set(engine, NULL)) {
			const HklGeometryList *geometries = hkl_engine_list_geometries_get(engines);
			const HklGeometryListItem *item;

			HKL_GEOMETRY_LIST_FOREACH(item, geometries){
				static double null[] = {0, 0, 0};

				hkl_engine_pseudo_axes_values_set(engine, null, 3, HKL_UNIT_DEFAULT, NULL);
				hkl_geometry_set(geometry,
						 hkl_geometry_list_item_geometry_get(item));
				hkl_engine_get(engine, NULL);
			}
		}
	}

	hkl_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
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

	ok(TRUE == TRUE, __func__);

	return 0;
}
