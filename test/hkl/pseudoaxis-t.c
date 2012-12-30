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
 * Copyright (C) 2003-2011 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <alloca.h>
#include <string.h>
#include <hkl.h>
#include <tap/basic.h>

#define with_log 1

static int test_engine(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	uint i;
	double values[10]; /* this should be the number of pseudo_axis */
	int unreachable = 0;
	int ko = HKL_FALSE;
	HklMode **mode;
	const HklGeometryList *geometries = hkl_engine_list_geometries(engine_list);
	HklGeometry *geometry = hkl_engine_list_get_geometry(engine_list);
	darray_mode *modes = hkl_engine_modes(engine);
	darray_parameter *pseudo_axes = (darray_parameter *)hkl_engine_pseudo_axes(engine);

	/* randomize the geometry */
	hkl_geometry_randomize(geometry);

	darray_foreach(mode, *modes){
		hkl_engine_select_mode(engine, *mode);
		/* for now unactive the eulerians check */
		if(!strcmp((*mode)->info->name, "eulerians"))
			continue;
		unreachable = 0;

		for(i=0;i<n && !ko;++i) {
			unsigned int j = 0;
			HklParameter **pseudo_axis;

			/* randomize the pseudoAxes values */
			darray_foreach(pseudo_axis, *pseudo_axes){
				hkl_parameter_randomize(*pseudo_axis);
				values[j++] = (*pseudo_axis)->_value;
			}

			/* randomize the parameters */
			hkl_parameter_list_randomize(&(*mode)->parameters);

			/* pseudo -> geometry */
			hkl_engine_initialize(engine, NULL);
			/* hkl_engine_fprintf(stderr, engine); */

			/* geometry -> pseudo */
			if(hkl_engine_set(engine, NULL)) {
				HklGeometryListItem *item;

				list_for_each(&geometries->items, item, node){
					/* first modify the pseudoAxes values */
					/* to be sure that the result is the */
					/* computed result. */

					darray_foreach(pseudo_axis, *pseudo_axes){
						hkl_parameter_set_value(*pseudo_axis, 0., NULL);
					}

					hkl_geometry_init_geometry(geometry, item->geometry);
					hkl_engine_get(engine, NULL);

					j = 0;
					darray_foreach(pseudo_axis, *pseudo_axes){
						ko |= fabs(values[j] - (*pseudo_axis)->_value) >= HKL_EPSILON;
						++j;
					}
					if(ko)
						break;
				}
			}else
				unreachable++;
		}
#if with_log
		fprintf(stderr, "\n\"%s\" \"%s\" \"%s\"",
			geometry->config->name,
			hkl_engine_name(engine),
			(*mode)->info->name);
		fprintf(stderr, " unreachable : %d/%d", unreachable, i);
		if(ko){
			fprintf(stderr, " ko");
			/* print the hkl internals if the test failed */
			fprintf(stderr, "\n    expected : ");
			for(uint j=0; j<darray_size(*pseudo_axes); ++j)
				fprintf(stderr, " %f", values[j]);
			fprintf(stderr, " obtained : ");
			for(uint j=0; j<darray_size(*pseudo_axes); ++j)
				fprintf(stderr, " %f",
					hkl_parameter_get_value(darray_item(*pseudo_axes, j)));
			hkl_engine_fprintf(stdout, engine);
		}else{
			fprintf(stderr, " ok");
		}
#endif
	}

	return !ko;
}

static int test_engines(HklEngineList *engine_list, int n)
{
	int res = HKL_TRUE;
	HklEngine **engine;
	darray_engine *engines = hkl_engine_list_engines(engine_list);

	darray_foreach(engine, *engines){
		res &= test_engine(*engine, engine_list, n);
	}

#if with_log
	fprintf(stderr, "\n");
#endif
	return res;
}

static void factories(void)
{
	int res = HKL_TRUE;
	uint i;
	const HklGeometryConfig *config;
	HklEngineList *engines;
	HklGeometryType types[] = {
		HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL,
		HKL_GEOMETRY_TYPE_EULERIAN6C,
		HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL,
		HKL_GEOMETRY_TYPE_KAPPA6C,
	};

	for(i=0; i<ARRAY_SIZE(types); ++i){
		config = hkl_geometry_factory_get_config_from_type(types[i]);
		engines = hkl_engine_list_factory(config);
		hkl_engine_list_free(engines);
	}

	ok(res == HKL_TRUE, "factories");
}

static void set(int n)
{
	const HklGeometryConfig *config;
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	HklEngineList *engines;
	int res = HKL_TRUE;

	/* attach to the second holder */
	detector->idx = 1;

	/* test all E4CV engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all E6C HKL engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN6C);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all K4CV HKL engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all K6C engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all ZAXIS engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_ZAXIS);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all SOLEIL SIXS MED 2+2 engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all SOLEIL MARS engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_SOLEIL_MARS);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all SOLEIL SIXS MED 1+2 engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all PETRA3 P09 EH2 engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_PETRA3_P09_EH2);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	/* test all E4CH engines */
	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL);
	geometry = hkl_geometry_factory_new(config);
	engines = hkl_engine_list_factory(config);
	hkl_engine_list_init(engines, geometry, detector, sample);
	res &= test_engines(engines, n);
	hkl_geometry_free(geometry);
	hkl_engine_list_free(engines);

	hkl_detector_free(detector);
	hkl_sample_free(sample);

	ok(res == HKL_TRUE, "set");
}

int main(int argc, char** argv)
{
	double n;

	plan(2);

	if (argc > 1)
		n = atoi(argv[1]);
	else
		n = 10;

	factories();
	set(n);

	return 0;
}
