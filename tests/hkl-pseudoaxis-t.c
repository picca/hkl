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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
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


typedef int (* test_func) (HklEngine *engine, HklEngineList *engine_list, unsigned int n);

static int __test(unsigned int nb_iter, test_func f, int foreach_mode)
{
	HklFactory **factories;
	size_t i, j, n;
	HklGeometry *geometry = NULL;
	HklDetector *detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	HklSample *sample = hkl_sample_new("test");
	HklEngineList *engines;
	int res = TRUE;
	const char **mode;

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
					res &= DIAG(hkl_engine_current_mode_set(*engine, *mode, NULL));
					for(j=0; j<nb_iter; ++j){
						res &= DIAG(f(*engine, engines, nb_iter));
						if(!res){
							diag("failed at factory: \"%s\" engine: \"%s\" mode: \"%s\"",
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
					res &= DIAG(f(*engine, engines, nb_iter));
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

#define TEST_FOREACH_ENGINE(_nb_iter, _f) __test(_nb_iter, _f, 0)
#define TEST_FOREACH_MODE(_nb_iter, _f) __test(_nb_iter, _f, 1)

static void factories(void)
{
	int res = TRUE;
	size_t i, n;
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
	const darray_string *pseudo_axes = hkl_engine_pseudo_axis_names_get(engine);
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
	res &= DIAG(hkl_engine_pseudo_axis_values_get(engine, currents, n_pseudo_axes,
						      HKL_UNIT_DEFAULT, NULL));

	/* idem with error management */
	error = NULL;
	res &= DIAG(hkl_engine_pseudo_axis_values_get(engine, currents, n_pseudo_axes,
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
	const darray_string *pseudo_axes = hkl_engine_pseudo_axis_names_get(engine);
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
	res &= DIAG(hkl_engine_initialized_set(engine, TRUE, &error));

	/* geometry -> pseudo */
	solutions = hkl_engine_pseudo_axis_values_set(engine,
						      targets, n_pseudo_axes,
						      HKL_UNIT_DEFAULT, &error);
	if(solutions) {
		const HklGeometryListItem *item;

		HKL_GEOMETRY_LIST_FOREACH(item, solutions){
			hkl_geometry_set(geometry,
					 hkl_geometry_list_item_geometry_get(item));

			res &= DIAG(hkl_engine_pseudo_axis_values_get(engine, currents, n_pseudo_axes, HKL_UNIT_DEFAULT, &error));
			for(j=0; j<n_pseudo_axes; ++j)
				res &= DIAG(fabs(targets[j] - currents[j]) < HKL_EPSILON);
		}
		hkl_geometry_list_free(solutions);
	}else{
		res &= DIAG(error != NULL);
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
	const darray_string *pseudo_axis_names = hkl_engine_pseudo_axis_names_get(engine);
	const char **pseudo_axis_name;
	const HklParameter *pseudo_axis;
	int res = TRUE;
	GError *error;

	darray_foreach(pseudo_axis_name, *pseudo_axis_names){
		pseudo_axis = hkl_engine_pseudo_axis_get(engine, *pseudo_axis_name, NULL);
		res &= DIAG(NULL != pseudo_axis);

		error = NULL;
		pseudo_axis = hkl_engine_pseudo_axis_get(engine, *pseudo_axis_name, &error);
		res &= DIAG(NULL != pseudo_axis);
		res &= DIAG(NULL == error);
	}

	/* error */
	pseudo_axis = hkl_engine_pseudo_axis_get(engine, bad, NULL);
	res &= DIAG(NULL == pseudo_axis);

	error = NULL;
	pseudo_axis = hkl_engine_pseudo_axis_get(engine, bad, &error);
	res &= DIAG(NULL == pseudo_axis);
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	return res;
}

static void pseudo_axis_get(void)
{
	ok(TRUE == TEST_FOREACH_ENGINE(1, _pseudo_axis_get), __func__);
}

static int _capabilities(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	int res = TRUE;
	const unsigned long capabilities = hkl_engine_capabilities_get(engine);

	/* all motors must have the read/write capabilities */
	res &= DIAG((capabilities & HKL_ENGINE_CAPABILITIES_READABLE) != 0);
	res &= DIAG((capabilities & HKL_ENGINE_CAPABILITIES_WRITABLE) != 0);

	/* all psi engines must be initialisable */
	if(!strcmp("psi", hkl_engine_name_get(engine)))
		res &= DIAG((capabilities & HKL_ENGINE_CAPABILITIES_INITIALIZABLE) != 0);

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

	if(HKL_ENGINE_CAPABILITIES_INITIALIZABLE & capabilities){
		int tmp;

		res &= DIAG(TRUE == hkl_engine_initialized_set(engine, FALSE, NULL));
		res &= DIAG(FALSE == hkl_engine_initialized_get(engine));
		tmp = hkl_engine_initialized_set(engine, TRUE, NULL);
		res &= DIAG(tmp == hkl_engine_initialized_get(engine));

		res &= DIAG(TRUE == hkl_engine_initialized_set(engine, FALSE, &error));
		res &= DIAG(FALSE == hkl_engine_initialized_get(engine));
		res &= DIAG(NULL == error);
		tmp = hkl_engine_initialized_set(engine, TRUE, &error);
		res &= DIAG(tmp == hkl_engine_initialized_get(engine));
		if(tmp)
			res &= DIAG(NULL == error);
		else{
			res &= DIAG(NULL != error);
			g_clear_error(&error);
		}
	}else{
		/* non-initializable engine should not produce an error */
		res &= DIAG(TRUE == hkl_engine_initialized_get(engine));
		res &= DIAG(TRUE == hkl_engine_initialized_set(engine, TRUE, NULL));
		res &= DIAG(TRUE == hkl_engine_initialized_get(engine));
		res &= DIAG(TRUE == hkl_engine_initialized_set(engine, FALSE, NULL));
		res &= DIAG(TRUE == hkl_engine_initialized_get(engine));

		res &= DIAG(TRUE == hkl_engine_initialized_set(engine, TRUE, &error));
		res &= DIAG(TRUE == hkl_engine_initialized_get(engine));
		res &= DIAG(NULL == error);
		res &= DIAG(TRUE == hkl_engine_initialized_set(engine, FALSE, &error));
		res &= DIAG(TRUE == hkl_engine_initialized_get(engine));
		res &= DIAG(NULL == error);
	}

	return res;
}

static void initialized(void)
{
	ok(TRUE == TEST_FOREACH_MODE(1, _initialized), __func__);
}

HKLAPI int hkl_engine_initialized_set(HklEngine *self, int initialized,
				      GError **error) HKL_ARG_NONNULL(1) HKL_WARN_UNUSED_RESULT;

static int _modes(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	static const char *bad = "__bad_mode_name__";
	int res = TRUE;
	int ok;
	const darray_string *modes;
	const char **mode;
	const char *current_mode;
	GError *error = NULL;

	modes = hkl_engine_modes_names_get(engine);

	/* check that the current mode is available in the mode list. */
	current_mode = hkl_engine_current_mode_get(engine);
	ok = FALSE;
	darray_foreach(mode, *modes){
		if(!strcmp(current_mode, *mode))
			ok = TRUE;
	}
	res &= DIAG(TRUE == ok);

	/* check that all modes can be set */
	darray_foreach(mode, *modes){
		res &= DIAG(TRUE == hkl_engine_current_mode_set(engine, *mode, NULL));

		res &= DIAG(TRUE == hkl_engine_current_mode_set(engine, *mode, &error));
		res &= DIAG(NULL == error);
	}

	/* check for bad mode name */
	res &= DIAG(FALSE == hkl_engine_current_mode_set(engine, bad, NULL));

	res &= DIAG(FALSE == hkl_engine_current_mode_set(engine, bad, &error));
	res &= DIAG(NULL != error);
	g_clear_error(&error);

	return res;
}

static void modes(void)
{
	ok(TRUE == TEST_FOREACH_ENGINE(1, _modes), __func__);
}

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

static int _axis_names(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	int res  = TRUE;
	HklGeometry *geometry;
	const darray_string *all_axes;
	const darray_string *axes_r;
	const darray_string *axes_w;

	geometry = hkl_engine_list_geometry_get(engine_list);
	all_axes = hkl_geometry_axis_names_get(geometry);

	/* check consistency of the engines, all axes should be in the
	 * list of the geometry axes */
	axes_r = hkl_engine_axis_names_get(engine,
					   HKL_ENGINE_AXIS_NAMES_GET_READ);

	axes_w = hkl_engine_axis_names_get(engine,
					   HKL_ENGINE_AXIS_NAMES_GET_WRITE);

	res &= DIAG(axes_r != NULL);
	res &= DIAG(axes_w != NULL);
	res &= DIAG(_check_axes(axes_r, all_axes));
	res &= DIAG(_check_axes(axes_w, all_axes));
}

static void axis_names(void)
{
	ok(TRUE == TEST_FOREACH_MODE(1, _axis_names), __func__);
}



static int _parameters(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	static const char *bad = "__bad_parameer_name__";
	int res = TRUE;
	GError *error = NULL;
	const char **parameter;
	const darray_string *parameters = hkl_engine_parameters_names_get(engine);
	const HklParameter *param;
	int n_values = darray_size(*parameters);
	double values[n_values];

	/* can not get a bad parameter */
	param = hkl_engine_parameter_get(engine, bad, NULL);
	res &= DIAG(NULL == param);

	param = hkl_engine_parameter_get(engine, bad, &error);
	res &= DIAG(NULL == param);
	res &= DIAG(NULL != error);
	g_clear_error(&error);

	/* it is possible to get and set all the parameters */
	darray_foreach(parameter, *parameters){
		/* get */
		param = hkl_engine_parameter_get(engine, *parameter, NULL);
		res &= DIAG(NULL != param);

		param = hkl_engine_parameter_get(engine, *parameter, &error);
		res &= DIAG(NULL != param);
		res &= DIAG(NULL == error);

		/* set */
		res &= DIAG(TRUE == hkl_engine_parameter_set(engine, *parameter, param, NULL));
		res &= DIAG(TRUE == hkl_engine_parameter_set(engine, *parameter, param, &error));
		res &= DIAG(NULL == error);

		res &= DIAG(FALSE == hkl_engine_parameter_set(engine, bad, param, &error));
		res &= DIAG(NULL != error);
		g_clear_error(&error);
	}

	hkl_engine_parameters_values_get(engine, values, n_values, HKL_UNIT_USER);

	res &= DIAG(TRUE == hkl_engine_parameters_values_set(engine, values, n_values, HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(TRUE == hkl_engine_parameters_values_set(engine, values, n_values, HKL_UNIT_DEFAULT, &error));
	res &= DIAG(NULL == error);

	res &= DIAG(TRUE == hkl_engine_parameters_values_set(engine, values, n_values, HKL_UNIT_USER, NULL));
	res &= DIAG(TRUE == hkl_engine_parameters_values_set(engine, values, n_values, HKL_UNIT_USER, &error));
	res &= DIAG(NULL == error);

	return res;
}

static void parameters(void)
{
	ok(TRUE == TEST_FOREACH_MODE(1, _parameters), __func__);
}

static int _depends(HklEngine *engine, HklEngineList *engine_list, unsigned int n)
{
	int res = TRUE;
	const char *name = hkl_engine_name_get(engine);
	const unsigned int depends = hkl_engine_dependencies_get(engine);

	if(!strcmp("hkl", name))
		res &= DIAG((depends & (HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_ENERGY | HKL_ENGINE_DEPENDENCIES_SAMPLE)) != 0);
	if(!strcmp("eulerians", hkl_engine_name_get(engine))){
		res &= DIAG((depends & HKL_ENGINE_DEPENDENCIES_AXES) != 0);
		res &= DIAG((depends & HKL_ENGINE_DEPENDENCIES_ENERGY) == 0);
		res &= DIAG((depends & HKL_ENGINE_DEPENDENCIES_SAMPLE) == 0);
	}
	if(!strcmp("q", name)){
		res &= DIAG((depends &(HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_ENERGY)) != 0);
		res &= DIAG((depends & HKL_ENGINE_DEPENDENCIES_SAMPLE) == 0);
	}
	if(!strcmp("q2", name)){
		res &= DIAG((depends &(HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_ENERGY)) != 0);
		res &= DIAG((depends & HKL_ENGINE_DEPENDENCIES_SAMPLE) == 0);
	}
	if(!strcmp("qper_qpar", name)){
		res &= DIAG((depends & (HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_ENERGY)) != 0);
		res &= DIAG((depends & HKL_ENGINE_DEPENDENCIES_SAMPLE) == 0);
	}
	if(!strcmp("psi", name))
		res &= DIAG((depends & (HKL_ENGINE_DEPENDENCIES_AXES | HKL_ENGINE_DEPENDENCIES_ENERGY | HKL_ENGINE_DEPENDENCIES_SAMPLE)) != 0);

	return res;
}

static void depends(void)
{
	ok(TRUE == TEST_FOREACH_ENGINE(1, _depends), __func__);
}

int main(int argc, char** argv)
{
	double n;

	plan(10);

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
	modes();
	axis_names();
	parameters();
	depends();

	return 0;
}
