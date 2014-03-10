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
#include <stdio.h>
#include <sys/time.h>
#include <tap/basic.h>
#include "hkl.h"

static void hkl_test_bench_run_real(HklEngine *engine, HklGeometry *geometry, size_t n)
{
	size_t i;
	HklMode **mode;
	darray_mode *modes = hkl_engine_modes_get(engine);

	/* pseudo -> geometry */
	darray_foreach(mode, *modes){
		double min, max, mean;
		darray_parameter *parameters;

		hkl_engine_select_mode(engine, *mode);
		parameters = hkl_mode_parameters_get(*mode);
		if (darray_size(*parameters))
			hkl_parameter_value_set(darray_item(*parameters, 0), 1, NULL);

		mean = max = 0;
		min = 1000; /* arbitrary value always greater than the real min */
		for(i=0; i<n; ++i){
			struct timeval debut, fin, dt;
			double t;

			hkl_geometry_set_values_unit_v(geometry, 0, 0, 0, 0, 10, 10);
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
			hkl_mode_name_get(*mode), n, i, min, mean/n, max);
	}
}

static void hkl_test_bench_run_v(HklEngineList *engines, HklGeometry *geometry,
				 char const *name, int n, ...)
{
	va_list ap;
	HklEngine *engine = hkl_engine_list_get_by_name(engines, name);
	HklParameter **pseudo_axis;
	darray_parameter *pseudo_axes = (darray_parameter *)hkl_engine_pseudo_axes_get(engine);

	va_start(ap, n);
	/* TODO replace with a specialise HklParameterList */
	darray_foreach(pseudo_axis, *pseudo_axes){
		hkl_parameter_value_set(*pseudo_axis,
					va_arg(ap, double), NULL);
	}
	va_end(ap);

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

	factory = hkl_factory_get_by_name("K6C");

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
	HklMode **mode;
	darray_mode *modes;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;

	factory = hkl_factory_get_by_name("K6C");

	geometry = hkl_factory_create_new_geometry(factory);
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");
	engines = hkl_factory_create_new_engine_list(factory);
	hkl_engine_list_init(engines, geometry, detector, sample);

	engine = hkl_engine_list_get_by_name(engines, "eulerians");
	modes = hkl_engine_modes_get(engine);

	darray_foreach(mode, *modes){
		static double eulerians[] = {0, 90 * HKL_DEGTORAD, 0};
		HklParameterList *pseudo_axes = hkl_engine_pseudo_axes_get(engine);

		hkl_engine_select_mode(engine, *mode);

		/* studdy this degenerated case */
		hkl_parameter_list_values_set(pseudo_axes, eulerians, 3, NULL);
		if (hkl_engine_set(engine, NULL)) {
			const HklGeometryList *geometries = hkl_engine_list_geometries(engines);
			const darray_item *items = hkl_geometry_list_items_get(geometries);
			HklGeometryListItem **item;

			darray_foreach(item, *items){
				static double null[] = {0, 0, 0};

				hkl_parameter_list_values_set(pseudo_axes, null, 3, NULL);
				hkl_geometry_set(geometry,
						 hkl_geometry_list_item_geometry_get(*item));
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

	ok(HKL_TRUE == HKL_TRUE, __func__);

	return 0;
}
