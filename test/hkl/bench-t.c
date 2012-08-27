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
#include <tap/basic.h>
#include <hkl.h>

static void hkl_test_bench_run_real(HklPseudoAxisEngine *engine, HklGeometry *geometry, size_t n)
{
	size_t i;
	HklPseudoAxisEngineMode *mode;

	/* pseudo -> geometry */
	list_for_each(&engine->modes, mode, list){
		double min, max, mean;

		hkl_pseudo_axis_engine_select_mode(engine, mode);
		if (mode->parameters_len)
			hkl_parameter_set_value(&mode->parameters[0], 1., NULL);

		mean = max = 0;
		min = 1000; /* arbitrary value always greater than the real min */
		for(i=0; i<n; ++i){
			struct timeval debut, fin, dt;
			double t;

			hkl_geometry_set_values_unit_v(geometry, 0, 0, 0, 0, 10, 10);
			gettimeofday(&debut, NULL);
			hkl_pseudo_axis_engine_set(engine, NULL);
			gettimeofday(&fin, NULL);
			timersub(&fin, &debut, &dt);
			t = dt.tv_sec * 1000. + dt.tv_usec / 1000.;
			min = t < min ? t : min;
			max = t > max ? t : max;
			mean += t;
		}
		fprintf(stdout, "\"%s\" \"%s\" \"%s\" (%d/%d) iterations %f / %f / %f [min/mean/max] ms each\n",
			geometry->config->name, engine->info->name,
			mode->info->name, n, i, min, mean/n, max);
	}
}

static void hkl_test_bench_run_v(HklPseudoAxisEngineList *engines, HklGeometry *geometry,
				 char const *name, int n, ...)
{
	HklPseudoAxisEngine *engine;
	HklParameter *parameter;
	va_list ap;

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, name);

	va_start(ap, n);
	/* TODO replace with a specialise HklParameterList */
	list_for_each(&engine->pseudo_axes.parameters, parameter, list){
		hkl_parameter_set_value(parameter, va_arg(ap, double), NULL);
	}
	va_end(ap);

	hkl_test_bench_run_real(engine, geometry, n);
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

	hkl_test_bench_run_v(engines, geom, "hkl", n, 1., 0., 0.);
	hkl_test_bench_run_v(engines, geom, "eulerians", n, 0., 90*HKL_DEGTORAD, 0.);
	hkl_test_bench_run_v(engines, geom, "psi", n, 10.*HKL_DEGTORAD);
	hkl_test_bench_run_v(engines, geom, "q2", n, 1., 10.*HKL_DEGTORAD);

	hkl_pseudo_axis_engine_list_free(engines);
	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void hkl_test_bench_eulerians(void)
{
	HklPseudoAxisEngineList *engines;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineMode *mode;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklDetector *detector;
	HklSample *sample;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geom = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

	engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "eulerians");

	list_for_each(&engine->modes, mode, list){
		static double eulerians[] = {0, 90 * HKL_DEGTORAD, 0};

		hkl_pseudo_axis_engine_select_mode(engine, mode);

		/* studdy this degenerated case */
		hkl_parameter_list_set_values(&engine->pseudo_axes, eulerians, 3, NULL);
		if (hkl_pseudo_axis_engine_set(engine, NULL)) {
			HklGeometryListItem *item;

			list_for_each(&engines->geometries->items, item, node){
				static double null[] = {0, 0, 0};

				hkl_parameter_list_set_values(&engine->pseudo_axes, null, 3, NULL);
				hkl_geometry_init_geometry(engine->geometry,
							   item->geometry);
				hkl_pseudo_axis_engine_get(engine, NULL);
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
