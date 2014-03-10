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
		if(!strcmp(hkl_mode_name_get(*mode), "eulerians"))
			continue;
		unreachable = 0;

		for(i=0;i<n && !ko;++i) {
			unsigned int j = 0;
			HklParameter **pseudo_axis;

			/* randomize the pseudoAxes values */
			darray_foreach(pseudo_axis, *pseudo_axes){
				hkl_parameter_randomize(*pseudo_axis);
				values[j++] = hkl_parameter_value_get(*pseudo_axis);
			}

			/* randomize the parameters */
			hkl_parameter_list_randomize(hkl_mode_parameters_get(*mode));

			/* pseudo -> geometry */
			hkl_engine_initialize(engine, NULL);
			/* hkl_engine_fprintf(stderr, engine); */

			/* geometry -> pseudo */
			if(hkl_engine_set(engine, NULL)) {
				const darray_item *items = hkl_geometry_list_items_get(geometries);
				HklGeometryListItem **item;

				darray_foreach(item, *items){
					/* first modify the pseudoAxes values */
					/* to be sure that the result is the */
					/* computed result. */

					darray_foreach(pseudo_axis, *pseudo_axes){
						hkl_parameter_value_set(*pseudo_axis, 0., NULL);
					}

					hkl_geometry_set(geometry,
							 hkl_geometry_list_item_geometry_get(*item));
					hkl_engine_get(engine, NULL);

					j = 0;
					darray_foreach(pseudo_axis, *pseudo_axes){
						ko |= fabs(values[j] - hkl_parameter_value_get(*pseudo_axis)) >= HKL_EPSILON;
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
			hkl_geometry_name_get(geometry),
			hkl_engine_name_get(engine),
			hkl_mode_name_get(*mode));
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
					hkl_parameter_value_get(darray_item(*pseudo_axes, j)));
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
