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
#include <alloca.h>
#include <string.h>
#include "hkl.h"
#include <tap/basic.h>
#include <tap/hkl-tap.h>

#define DEBUG
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

/* BEWARE here we are using a GCC extension */
#define DIAG(_success)							\
	({	typeof(_success) __success = (_success);		\
		if(!__success)						\
			diag("status: %d " __FILE__ ":" TOSTRING(__LINE__) ":%s", (__success) , __func__); \
		__success;						\
	})


typedef int (* test_func) (HklEngine *engine, HklEngineList *engine_list, unsigned int n);

static int __test(unsigned int nb_iter, test_func f, int foreach_mode)
{
	HklFactory **factories;
	unsigned int i, j, n;
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test");
	HklEngineList *engines;
	int res = TRUE;
	const char **mode;

	/* attach to the second holder */
	hkl_detector_idx_set(detector, 1);

	factories = hkl_factory_get_all(&n);
	for(i=0; i<n && TRUE == res; i++){
		HklEngine **engine;

		geometry = hkl_factory_create_new_geometry(factories[i]);
		engines = hkl_factory_create_new_engine_list(factories[i]);
		hkl_engine_list_init(engines, geometry, detector, sample);
		darray_foreach(engine, *hkl_engine_list_engines_get(engines)){
			const darray_string *modes = hkl_engine_modes_names_get(*engine);
			if (foreach_mode){
				darray_foreach(mode, *modes){
					res &= hkl_engine_current_mode_set(*engine, *mode, NULL);
					for(j=0; j<nb_iter; ++j){
						res &= f(*engine, engines, nb_iter);
						if(!res){
							diag("failed at factory: %s engine: %s mode: %s",
							     hkl_geometry_name_get(geometry),
							     hkl_engine_name_get(*engine), *mode);
							break;
						}
					}
					if(!res)
						break;
				}
			}else{
				for(j=0; j<nb_iter; ++j){
					res &= f(*engine, engines, nb_iter);
					if(!res)
						break;
				}
			}
			if(!res)
				break;
		}
		hkl_geometry_free(geometry);
		hkl_engine_list_free(engines);
	}
	hkl_detector_free(detector);
	hkl_sample_free(sample);

	return res;
}

#define TEST(_nb_iter, _f) __test(_nb_iter, _f, 0)
#define TEST_FOREACH_MODE(_nb_iter, _f) __test(_nb_iter, _f, 1)

static void factories(void)
{
	int res = TRUE;
	uint i, n;
	HklEngineList *engines;
	HklFactory **factories;

	factories = hkl_factory_get_all(&n);
	for(i=0;i<n; i++){
		engines = hkl_factory_create_new_engine_list(factories[i]);
		hkl_engine_list_free(engines);
	}

	ok(res == TRUE, "factories");
}

static int _get(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	uint i;
	GError *error;
	int res = TRUE;
	HklGeometry *geometry = hkl_engine_list_geometry_get(engine_list);
	const darray_string *pseudo_axes = hkl_engine_pseudo_axes_names_get(engine);
	const size_t n_pseudo_axes = darray_size(*pseudo_axes);
	double targets[n_pseudo_axes];
	double currents[n_pseudo_axes];

	/* randomize the geometry */
	hkl_geometry_randomize(geometry);

	/* randomize the pseudoAxes values */
	hkl_tap_engine_pseudo_axes_randomize(engine,
					     targets, n_pseudo_axes,
					     HKL_UNIT_DEFAULT);

	/* randomize the parameters */
	hkl_tap_engine_parameters_randomize(engine);

	/* pseudo -> geometry */
	if(HKL_ENGINE_CAPABILITIES_INITIALIZABLE & hkl_engine_capabilities_get(engine))
		res &= DIAG(hkl_engine_initialized_set(engine, TRUE, NULL));
	res &= DIAG(hkl_engine_pseudo_axes_values_get(engine, currents, n_pseudo_axes,
						      HKL_UNIT_DEFAULT, NULL));

	/* idem with error management */
	error = NULL;
	res &= DIAG(hkl_engine_pseudo_axes_values_get(engine, currents, n_pseudo_axes,
						      HKL_UNIT_DEFAULT, &error));
	res &= DIAG(NULL == error);
	for(i=0; i<n_pseudo_axes; ++i)
		res &= DIAG(targets[i] != currents[i]); /* TODO this test is almost true, need a real check */

	return res;
}

static void get()
{
	ok(TRUE == TEST_FOREACH_MODE(1, _get), __func__);
}

static int _set(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	uint i;
	GError *error = NULL;
	int unreachable = 0;
	int res = TRUE;
	HklGeometry *geometry = hkl_engine_list_geometry_get(engine_list);
	const darray_string *pseudo_axes = hkl_engine_pseudo_axes_names_get(engine);
	const size_t n_pseudo_axes = darray_size(*pseudo_axes);
	double targets[n_pseudo_axes];
	double currents[n_pseudo_axes];

	/* randomize the geometry */
	hkl_geometry_randomize(geometry);

	/* for now skip the eulerians check */
	if(!strcmp(hkl_engine_current_mode_get(engine), "eulerians"))
		return TRUE;

	size_t j;
	HklParameter **pseudo_axis;
	HklGeometryList *solutions;

	/* randomize the pseudoAxes values */
	hkl_tap_engine_pseudo_axes_randomize(engine,
					     targets, n_pseudo_axes,
					     HKL_UNIT_DEFAULT);

	/* randomize the parameters */
	hkl_tap_engine_parameters_randomize(engine);

	/* pseudo -> geometry */
	res &= hkl_engine_initialized_set(engine, TRUE, &error);

	/* geometry -> pseudo */
	solutions = hkl_engine_pseudo_axes_values_set(engine,
						      targets, n_pseudo_axes,
						      HKL_UNIT_DEFAULT, &error);
	if(solutions) {
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, solutions){
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));

			res &= hkl_engine_pseudo_axes_values_get(engine,
								 currents, n_pseudo_axes,
								 HKL_UNIT_DEFAULT, &error);
			for(j=0; j<n_pseudo_axes; ++j)
				res &= fabs(targets[j] - currents[j]) < HKL_EPSILON;
		}
	}else{
		res &= error != NULL;
		g_clear_error(&error);
		unreachable++;

#if 0
		fprintf(stderr, " unreachable : %d/%d", unreachable, i);
		if(!res){
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
	return res;
}


static void set(int nb_iter)
{
	ok(TRUE == TEST_FOREACH_MODE(nb_iter, _set), __func__);
}


static int _pseudo_axis_get(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	static const char *bad = "_bad_name_";
	const darray_string *pseudo_axes_names = hkl_engine_pseudo_axes_names_get(engine);
	const char **pseudo_axis_name;
	const HklParameter *pseudo_axis;
	int res = TRUE;
	GError *error;

	darray_foreach(pseudo_axis_name, *pseudo_axes_names){
		pseudo_axis = hkl_engine_pseudo_axis_get(engine, *pseudo_axis_name, NULL);
		res &= NULL != pseudo_axis;

		error = NULL;
		pseudo_axis = hkl_engine_pseudo_axis_get(engine, *pseudo_axis_name, &error);
		res &= NULL != pseudo_axis;
		res &= NULL == error;
	}

	/* error */
	pseudo_axis = hkl_engine_pseudo_axis_get(engine, bad, NULL);
	res &= NULL == pseudo_axis;

	error = NULL;
	pseudo_axis = hkl_engine_pseudo_axis_get(engine, bad, &error);
	res &= NULL == pseudo_axis;
	res &= error != NULL;
	g_clear_error(&error);

	return res;
}

static void pseudo_axis_get(void)
{
	ok(TRUE == TEST(1, _pseudo_axis_get), __func__);
}

static int _capabilities(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	int res = TRUE;
	const unsigned long capabilities = hkl_engine_capabilities_get(engine);

	/* all motors must have the read/write capabilities */
	res &= (capabilities & HKL_ENGINE_CAPABILITIES_READABLE) != 0;
	res &= (capabilities & HKL_ENGINE_CAPABILITIES_WRITABLE) != 0;

	/* all psi engines must be initialisable */
	if(!strcmp("psi", hkl_engine_name_get(engine)))
		res &= (capabilities & HKL_ENGINE_CAPABILITIES_INITIALIZABLE) != 0;

	return res;
}

static void capabilities(void)
{
	ok(TRUE == TEST_FOREACH_MODE(1, _capabilities), __func__);
}

static int _initialized(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	int res = TRUE;
	GError *error = NULL;
	const unsigned long capabilities = hkl_engine_capabilities_get(engine);

	/* all psi engines must be initialisable */
	if(!strcmp("psi", hkl_engine_name_get(engine))){
		/* res &= (capabilities & HKL_ENGINE_CAPABILITIES_INITIALIZABLE) != 0; */
		/* /\* first it must not be initialized *\/ */
		/* res &= FALSE == hkl_engine_initialized_get(engine); */

		/* res &= TRUE == hkl_engine_initialized_set(engine, FALSE, NULL); */
		/* res &= FALSE == hkl_engine_initialized_get(engine); */
		/* res &= TRUE == hkl_engine_initialized_set(engine, TRUE, NULL); */
		/* res &= TRUE == hkl_engine_initialized_get(engine);		 */

		/* res &= TRUE == hkl_engine_initialized_set(engine, FALSE, &error); */
		/* res &= FALSE == hkl_engine_initialized_get(engine); */
		/* res &= NULL == error; */
		/* res &= TRUE == hkl_engine_initialized_set(engine, TRUE, &error); */
		/* res &= TRUE == hkl_engine_initialized_get(engine);		 */
		/* res &= NULL == error; */
	}else{
		/* non-initializable engine should not produce an error */
		res &= TRUE == hkl_engine_initialized_set(engine, TRUE, NULL);
		res &= TRUE == hkl_engine_initialized_get(engine);
		res &= TRUE == hkl_engine_initialized_set(engine, FALSE, NULL);
		res &= TRUE == hkl_engine_initialized_get(engine);

		res &= TRUE == hkl_engine_initialized_set(engine, TRUE, &error);
		res &= TRUE == hkl_engine_initialized_get(engine);
		res &= NULL == error;
		res &= TRUE == hkl_engine_initialized_set(engine, FALSE, &error);
		res &= TRUE == hkl_engine_initialized_get(engine);
		res &= NULL == error;
	}

	return res;
}

static void initialized(void)
{
	ok(TRUE == TEST_FOREACH_MODE(1, _initialized), __func__);
}

HKLAPI int hkl_engine_initialized_set(HklEngine *self, int initialized,
				      GError **error) HKL_ARG_NONNULL(1) HKL_WARN_UNUSED_RESULT;

static int _check_axes(const darray_string *axes, const darray_string *refs)
{
	int ko = TRUE;
	const char **axis;
	const char **ref;

	darray_foreach(axis, *axes){
		darray_foreach(ref, *refs){
			if(!strcmp(*axis, *ref))
				ko = FALSE;
		}
		if(ko)
			break;
		ko = TRUE;
	}
	return ko;
}

static void axes_names_get(void)
{
	HklFactory **factories;
	unsigned int i, n;
	int res = TRUE;

	factories = hkl_factory_get_all(&n);
	for(i=0; i<n; i++){
		HklGeometry *geometry;
		HklEngineList *list;
		HklEngine **engine;
		darray_engine *engines;
		const darray_string *all_axes;

		geometry = hkl_factory_create_new_geometry(factories[i]);
		list = hkl_factory_create_new_engine_list(factories[i]);
		engines = hkl_engine_list_engines_get(list);
		all_axes = hkl_geometry_axes_names_get(geometry);

		/* check consistency of the engines, all axes should
		 * be in the list of the geometry axes */
		darray_foreach(engine, *engines){
			const char **axis;
			const darray_string *axes_r = hkl_engine_axes_names_get(*engine,
										HKL_ENGINE_AXES_NAMES_GET_READ);

			const darray_string *axes_w = hkl_engine_axes_names_get(*engine,
										HKL_ENGINE_AXES_NAMES_GET_WRITE);

			res &= axes_r != NULL;
			res &= axes_w != NULL;
			res &= _check_axes(axes_r, all_axes);
			res &= _check_axes(axes_w, all_axes);
		}
		hkl_engine_list_free(list);
		hkl_geometry_free(geometry);
	}

	ok(res == TRUE, __func__);
}

int main(int argc, char** argv)
{
	double n;

	plan(7);

	if (argc > 1)
		n = atoi(argv[1]);
	else
		n = 10;

	factories();
	get();
	set(n);
	pseudo_axis_get();
	capabilities();
	initialized();
	axes_names_get();

	return 0;
}
