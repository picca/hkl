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

#define with_log 0
#define N 2

static int test_engine(HklPseudoAxisEngine *engine, HklGeometry *geometry,
		       HklDetector *detector, HklSample *sample)
{
	size_t i, j, k, f_idx;
	double *values = alloca(engine->pseudoAxes_len * sizeof(*values));
	int miss = 0;

	/* randomize the geometry */
	hkl_geometry_randomize(geometry);
	
	for(f_idx=0; f_idx<engine->modes_len; ++f_idx) {
		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		/* for now unactive the eulerians check */
		if(!strcmp(engine->mode->name, "eulerians"))
			continue;
		miss = 0;
		for(i=0;i<N;++i) {
			int res;
			size_t len = engine->pseudoAxes_len;

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
			res = hkl_pseudo_axis_engine_set(engine, NULL);

			/* geometry -> pseudo */
			if (res == HKL_SUCCESS) {
				size_t g_len = hkl_geometry_list_len(engine->engines->geometries);
				/* check all finded geometries */
				/* hkl_pseudo_axis_engine_fprintf(stderr, engine); */

				for(j=0; j<g_len; ++j) {
					/* first modify the pseudoAxes values */
					/* to be sure that the result is the */
					/* computed result. */
					for(k=0; k<len; ++k)
						((HklParameter *)engine->pseudoAxes[k])->value = 0.;

					hkl_geometry_init_geometry(geometry,
								   engine->engines->geometries->items[j].geometry);
					hkl_pseudo_axis_engine_get(engine, NULL);

					for(k=0; k<len; ++k) {
						is_double(values[k],
							  ((HklParameter *)engine->pseudoAxes[k])->value,
							  HKL_EPSILON, __func__);
					}
				}
			} else
				miss++;
		}

#if with_log
		fprintf(stderr, "\n\"%s\" \"%s\" missed : %d",
			engine->geometry->config->name,
			engine->mode->name, miss);
#endif

	}
	
#if with_log
	fprintf(stderr, "\n");
#endif
}

#define test_engines(engines) do{					\
		size_t i;						\
		for(i=0; i<engines->len; ++i)				\
			test_engine(engines->engines[i],		\
				    engines->geometry,			\
				    engines->detector,			\
				    engines->sample);			\
	}while(0)

static void set(void)
{
	const HklGeometryConfig *config;
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	HklPseudoAxisEngineList *engines;

	/* attach to the second holder */
	detector->idx = 1;

	/* test all E4CV engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all E6C HKL engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all K4CV HKL engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all K6C engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all ZAXIS engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_ZAXIS);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	/* test all E4CH engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	hkl_detector_free(detector);
	hkl_sample_free(sample);
}

int main(int argc, char** argv)
{
	plan(593);

	set();

	return 0;
}
