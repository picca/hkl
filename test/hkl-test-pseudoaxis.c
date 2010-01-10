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

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis

#define with_log 0
#define N 2

static int test_engine(struct hkl_test *test,
		       HklPseudoAxisEngine *engine, HklGeometry *geometry,
		       HklDetector *detector, HklSample *sample)
{
	size_t i, j, k, f_idx;
	double values[HKL_LIST_LEN(engine->pseudoAxes)];
	int miss = 0;

	// randomize the geometry
	hkl_geometry_randomize(geometry);
	
	for(f_idx=0; f_idx<HKL_LIST_LEN(engine->modes); ++f_idx) {
		hkl_pseudo_axis_engine_select_mode(engine, f_idx);
		// for now unactive the eulerians check
		if(!strcmp(engine->mode->name, "eulerians"))
			continue;
		miss = 0;
		for(i=0;i<N;++i) {
			int res;
			size_t len = HKL_LIST_LEN(engine->pseudoAxes);

			// randomize the pseudoAxes values
			for(j=0; j<len; ++j) {
				HklParameter *parameter = (HklParameter *)(engine->pseudoAxes[j]);
				hkl_parameter_randomize(parameter);

				values[j] = parameter->value;
			}

			// randomize the parameters
			for(j=0; j<HKL_LIST_LEN(engine->mode->parameters); ++j)
				hkl_parameter_randomize(&engine->mode->parameters[j]);

			// pseudo -> geometry
			hkl_pseudo_axis_engine_initialize(engine);
			//hkl_pseudo_axis_engine_fprintf(stderr, engine);
			res = hkl_pseudo_axis_engine_set(engine);

			// geometry -> pseudo
			if (res == HKL_SUCCESS) {
				size_t g_len = HKL_LIST_LEN(engine->engines->geometries->geometries);
				// check all finded geometries
				//hkl_pseudo_axis_engine_fprintf(stderr, engine);

				for(j=0; j<g_len; ++j) {
					// first modify the pseudoAxes values
					// to be sure that the result is the
					// computed result.
					for(k=0; k<len; ++k)
						((HklParameter *)engine->pseudoAxes[k])->value = 0.;

					hkl_geometry_init_geometry(geometry, engine->engines->geometries->geometries[j]);
					hkl_pseudo_axis_engine_get(engine);

					for(k=0; k<len; ++k) {
						HKL_ASSERT_DOUBLES_EQUAL(values[k],
									 ((HklParameter *)engine->pseudoAxes[k])->value,
									 HKL_EPSILON);
					}
				}
			} else
				miss++;
		}

#if with_log
		fprintf(stderr, "\n\"%s\" \"%s\" missed : %d",
			engine->geometry->name,
			engine->mode->name, miss);
#endif

	}
	
#if with_log
	fprintf(stderr, "\n");
#endif

	return HKL_TEST_PASS;
}

#define test_engines(test, engines) do{					\
		size_t i;						\
		for(i=0; i<HKL_LIST_LEN(engines->engines); ++i){	\
			if (!test_engine(test, engines->engines[i],	\
					 engines->geometry,		\
					 engines->detector,		\
					 engines->sample))		\
				return HKL_TEST_FAIL;			\
		}							\
	}while(0)

HKL_TEST_SUITE_FUNC(set)
{
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	HklPseudoAxisEngineList *engines;

	// attach to the second holder
	detector->idx = 1;

	// test all E4CV engines
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(test, engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all E6C HKL engines
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN6C);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_TYPE_EULERIAN6C);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(test, engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all K4CV HKL engines
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL, 50 * HKL_DEGTORAD);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(test, engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all K6C engines
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_KAPPA6C, 50 * HKL_DEGTORAD);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_TYPE_KAPPA6C);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(test, engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);

	// test all ZAXIS engines
	geometry = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_ZAXIS);
	engines = hkl_pseudo_axis_engine_list_factory(HKL_GEOMETRY_TYPE_ZAXIS);
	hkl_pseudo_axis_engine_list_init(engines, geometry, detector, sample);
	test_engines(test, engines);
	hkl_geometry_free(geometry);
	hkl_pseudo_axis_engine_list_free(engines);
	hkl_detector_free(detector);
	hkl_sample_free(sample);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( set );

HKL_TEST_SUITE_END
