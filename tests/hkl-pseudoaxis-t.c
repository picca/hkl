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
#include <alloca.h>
#include <string.h>
#include "hkl.h"
#include <tap/basic.h>

#define with_log 1

static int test_engine(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	uint i;
	int unreachable = 0;
	int ko = HKL_FALSE;
	const char **mode;
	const HklGeometryList *geometries = hkl_engine_list_geometries_get(engine_list);
	HklGeometry *geometry = hkl_engine_list_geometry_get(engine_list);
	const darray_string *modes = hkl_engine_modes_get(engine);
	const darray_string *pseudo_axes = hkl_engine_pseudo_axes_get(engine);
	const size_t n_pseudo_axes = darray_size(*pseudo_axes);
	double targets[n_pseudo_axes];
	double currents[n_pseudo_axes];
	double garbages[n_pseudo_axes];

	/* randomize the geometry */
	hkl_geometry_randomize(geometry);

	darray_foreach(mode, *modes){
		hkl_engine_select_mode(engine, *mode);
		/* for now unactive the eulerians check */
		if(!strcmp(*mode, "eulerians"))
			continue;
		unreachable = 0;

		for(i=0;i<n && !ko;++i) {
			size_t j;
			HklParameter **pseudo_axis;

			/* randomize the pseudoAxes values */
			hkl_engine_pseudo_axes_randomize(engine);
			hkl_engine_pseudo_axes_values_get(engine, targets, n_pseudo_axes);

			/* randomize the parameters */
			hkl_engine_parameters_randomize(engine);

			/* pseudo -> geometry */
			hkl_engine_initialize(engine, NULL);

			/* geometry -> pseudo */
			if(hkl_engine_set(engine, NULL)) {
				const HklGeometryListItem *item;

				HKL_GEOMETRY_LIST_FOREACH(item, geometries){
					/* first modify the pseudoAxes values */
					/* to be sure that the result is the */
					/* computed result. */

					hkl_engine_pseudo_axes_values_set(engine,
									  garbages, n_pseudo_axes,
									  NULL);

					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(item));
					hkl_engine_get(engine, NULL);

					hkl_engine_pseudo_axes_values_get(engine,
									  currents, n_pseudo_axes);
					for(j=0; j<n_pseudo_axes; ++j)
						ko |= fabs(targets[j] - currents[j]) >= HKL_EPSILON;
					if(ko)
						break;
				}
			}else
				unreachable++;
		}
#if with_log
		fprintf(stderr, "\n\"%s\" \"%s\" \"%s\"",
			hkl_geometry_name_get(geometry),
			hkl_engine_name_get(engine),
			*mode);
		fprintf(stderr, " unreachable : %d/%d", unreachable, i);
		if(ko){
			fprintf(stderr, " ko");
			/* print the hkl internals if the test failed */
			fprintf(stderr, "\n    expected : ");
			for(uint j=0; j<n_pseudo_axes; ++j)
				fprintf(stderr, " %f", targets[j]);
			fprintf(stderr, " obtained : ");
			for(uint j=0; j<n_pseudo_axes; ++j)
				fprintf(stderr, " %f", currents[j]);
			hkl_engine_fprintf(stderr, engine);
			exit(0);
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
	darray_engine *engines = hkl_engine_list_engines_get(engine_list);

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
	uint i, n;
	HklEngineList *engines;
	HklFactory **factories;

	factories = hkl_factory_get_all(&n);
	for(i=0;i<n; i++){
		engines = hkl_factory_create_new_engine_list(factories[i]);
		hkl_engine_list_free(engines);
	}

	ok(res == HKL_TRUE, "factories");
}

static void set(int nb_iter)
{
	HklFactory **factories;
	unsigned int i, n;
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test");
	HklEngineList *engines;
	int res = HKL_TRUE;

	/* attach to the second holder */
	hkl_detector_idx_set(detector, 1);

	factories = hkl_factory_get_all(&n);
	for(i=0; i<n; i++){
		geometry = hkl_factory_create_new_geometry(factories[i]);
		engines = hkl_factory_create_new_engine_list(factories[i]);
		hkl_engine_list_init(engines, geometry, detector, sample);
		res &= test_engines(engines, nb_iter);
		hkl_geometry_free(geometry);
		hkl_engine_list_free(engines);
	}

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
