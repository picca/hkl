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
#include <alloca.h>
#include <hkl.h>
#include <tap/basic.h>

#define with_log 1
#define N 4

static int test_engine(HklPseudoAxisEngine *engine, HklGeometry *geometry,
		       HklDetector *detector, HklSample *sample, int n)
{
	size_t i, j, k, f_idx;
	double *values = alloca(engine->pseudoAxes_len * sizeof(*values));
	int unreachable = 0;
	int ko = HKL_FALSE;

	/* randomize the geometry */
	hkl_geometry_randomize(geometry);
	
	for(f_idx=0; f_idx<engine->modes_len; ++f_idx) {
		size_t len;

		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		/* for now unactive the eulerians check */
		if(!strcmp(engine->mode->name, "eulerians"))
			continue;
		unreachable = 0;

		len = engine->pseudoAxes_len;
		for(i=0;i<n && !ko;++i) {
			/* randomize the pseudoAxes values */
			for(j=0; j<len; ++j) {
				HklParameter *parameter = (HklParameter *)(engine->pseudoAxes[j]);
				hkl_parameter_randomize(parameter);

				values[j] = parameter->value;
			}

			/* randomize the parameters */
			for(j=0; j<engine->mode->parameters_len; ++j)
				hkl_parameter_randomize(&engine->mode->parameters[j]);

			/* pseudo -> geometry */
			hkl_pseudo_axis_engine_initialize(engine, NULL);
			/* hkl_pseudo_axis_engine_fprintf(stderr, engine); */

			/* geometry -> pseudo */
			if(hkl_pseudo_axis_engine_set(engine, NULL)) {
				for(j=0; j<engine->engines->geometries->len && !ko; ++j) {
					/* first modify the pseudoAxes values */
					/* to be sure that the result is the */
					/* computed result. */
					for(k=0; k<len; ++k)
						((HklParameter *)engine->pseudoAxes[k])->value = 0.;

					hkl_geometry_init_geometry(geometry,
								   engine->engines->geometries->items[j].geometry);
					hkl_pseudo_axis_engine_get(engine, NULL);

					for(k=0; k<len; ++k)
						ko |= fabs(values[k] - ((HklParameter *)engine->pseudoAxes[k])->value) >= HKL_EPSILON;
				}
			}else
				unreachable++;
		}
#if with_log
		fprintf(stderr, "\n\"%s\" \"%s\" \"%s\"",
			engine->geometry->config->name,
			engine->name,
			engine->mode->name);
		fprintf(stderr, " unreachable : %d/%d", unreachable, i);
		if(ko){
			fprintf(stderr, " ko");
			/* print the hkl internals if the test failed */
			fprintf(stderr, "\n    expected : ");
			for(k=0; k<len; ++k)
				fprintf(stderr, " %f", values[k]);
			fprintf(stderr, " obtained : ");
			for(k=0; k<len; ++k)
				fprintf(stderr, " %f", ((HklParameter *)engine->pseudoAxes[k])->value);
			hkl_pseudo_axis_engine_fprintf(stdout, engine);
		}else{
			fprintf(stderr, " ok");
		}
#endif
	}

	return !ko;
}

static int test_engines(HklPseudoAxisEngineList *engines, int n)
{
	int res = HKL_TRUE;

	size_t i;
	for(i=0; i<engines->len; ++i)
		res &= test_engine(engines->engines[i],
				   engines->geometry,
				   engines->detector,
				   engines->sample,
				   n);
			
#if with_log
	fprintf(stderr, "\n");
#endif
	return res;
}

static void set(void)
{
	const HklGeometryConfig *config;
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	HklPseudoAxisEngineList *engines;
	int res = HKL_TRUE;

	/* attach to the second holder */
	detector->idx = 1;

	/* test all E4CV engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all E6C HKL engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all K4CV HKL engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all K6C engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all ZAXIS engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_ZAXIS);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all SOLEIL SIXS MED engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all E4CH engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all MARS engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_MARS);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, N);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	hkl_detector_free(detector);
	hkl_sample_free(sample);

	ok(res == HKL_TRUE, "set");
}

int main(int argc, char** argv)
{
	plan(1);

	set();

	return 0;
}
