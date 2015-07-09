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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

/* BEWARE THESE TESTS ARE DEALING WITH HKL INTERNALS WHICH EXPOSE A
 * NON PUBLIC API WHICH ALLOW TO SHOOT YOURSELF IN YOUR FOOT */

#include "hkl/ccan/container_of/container_of.h"
#include "hkl-axis-private.h" /* temporary */
#include "hkl-geometry-private.h"

static void add_holder(void)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;

	g = hkl_geometry_new(NULL);
	is_int(0, darray_size(g->holders), __func__);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	is_int(1, darray_size(g->holders), __func__);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);
	is_int(2, darray_size(g->holders), __func__);

	ok(holder == darray_item(g->holders, 1), __func__);

	hkl_geometry_free(g);
}

static void get_axis(void)
{
	int res = TRUE;
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	const HklParameter *axis0;
	GError *error;

	g = hkl_geometry_new(NULL);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	/* check the private API */
	res &= DIAG(0 == !hkl_geometry_get_axis_by_name(g, "A"));
	res &= DIAG(0 == !hkl_geometry_get_axis_by_name(g, "B"));
	res &= DIAG(0 == !hkl_geometry_get_axis_by_name(g, "C"));
	res &= DIAG(1 == !hkl_geometry_get_axis_by_name(g, "D"));

	/* check the public API */
	/* get */
	res &= DIAG(NULL != hkl_geometry_axis_get(g, "A", NULL));
	res &= DIAG(NULL == hkl_geometry_axis_get(g, "D", NULL));
	error = NULL;
	res &= DIAG(NULL != hkl_geometry_axis_get(g, "A", &error));
	res &= DIAG(error == NULL);
	res &= DIAG(NULL == hkl_geometry_axis_get(g, "D", &error));
	res &= DIAG(error != NULL);
	g_clear_error(&error);

	/* set */
	axis0 = hkl_geometry_axis_get(g, "A", NULL);
	res &= DIAG(TRUE == hkl_geometry_axis_set(g, "A", axis0, NULL));
	res &= DIAG(FALSE == hkl_geometry_axis_set(g, "B", axis0, NULL));

	error = NULL;
	res &= DIAG(hkl_geometry_axis_set(g, "A", axis0, &error));
	res &= DIAG(error == NULL);

	res &= DIAG(FALSE == hkl_geometry_axis_set(g, "B", axis0, &error));
	res &= DIAG(error != NULL);

	ok(res, __func__);

	g_clear_error(&error);

	hkl_geometry_free(g);
}

static void update(void)
{
	int res = TRUE;
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	HklAxis *axis1;

	g = hkl_geometry_new(NULL);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axis1 = container_of(hkl_geometry_get_axis_by_name(g, "B"), HklAxis, parameter);
	res &= DIAG(hkl_parameter_value_set(&axis1->parameter, M_PI_2, HKL_UNIT_DEFAULT, NULL));
	/* now axis1 is dirty */
	ok(TRUE == axis1->parameter.changed, __func__);

	hkl_geometry_update(g);
	holder = darray_item(g->holders, 0);
	is_double(1./sqrt(2), holder->q.data[0], HKL_EPSILON, __func__);
	is_double(1./sqrt(2), holder->q.data[1], HKL_EPSILON, __func__);
	is_double(.0, holder->q.data[2], HKL_EPSILON, __func__);
	is_double(.0, holder->q.data[3], HKL_EPSILON, __func__);
	/* now axis1 is clean */
	res &= DIAG(FALSE == axis1->parameter.changed);

	ok(res, __func__);

	hkl_geometry_free(g);
}

static void set(void)
{
	HklGeometry *g;
	HklGeometry *g1;
	HklGeometry *g2;
	HklHolder *holder;
	HklFactory *fake_factory;

	g = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	g1 = hkl_geometry_new_copy(g);

	/* it is required to use a fake factory, with the public API
	 * geometry contain always a real factory */
	fake_factory = (HklFactory *)0x1;
	g2 = hkl_geometry_new(fake_factory);
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	ok(hkl_geometry_set(g, g1), __func__);

	hkl_geometry_free(g2);
	hkl_geometry_free(g1);
	hkl_geometry_free(g);
}

static void axis_values_get_set(void)
{
	unsigned int i;
	HklGeometry *g;
	HklHolder *holder;
	static double set_1[] = {1, 1, 1};
	static double set_10[] = {10, 10, 10};
	double values[3];
	GError *error;

	g = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	/* check set DEFAULT unit */
	error = NULL;
	ok(TRUE == hkl_geometry_axis_values_set(g, set_1, ARRAY_SIZE(set_1), HKL_UNIT_DEFAULT, NULL), __func__);
	ok(TRUE == hkl_geometry_axis_values_set(g, set_1, ARRAY_SIZE(set_1), HKL_UNIT_DEFAULT, &error), __func__);
	ok(error == NULL, __func__);
	for(i=0; i<ARRAY_SIZE(set_1); ++i)
		is_double(set_1[i], hkl_parameter_value_get(darray_item(g->axes, i), HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);

	/* check get DEFAULT unit */
	hkl_geometry_axis_values_get(g, values, 3, HKL_UNIT_DEFAULT);
	for(i=0; i<ARRAY_SIZE(set_1); ++i)
		is_double(set_1[i], values[i], HKL_EPSILON, __func__);

	/* check set USER unit */
	ok(TRUE == hkl_geometry_axis_values_set(g, set_10, ARRAY_SIZE(set_10), HKL_UNIT_USER, NULL), __func__);
	ok(TRUE == hkl_geometry_axis_values_set(g, set_10, ARRAY_SIZE(set_10), HKL_UNIT_USER, &error), __func__);
	ok(error == NULL, __func__);
	for(i=0; i<ARRAY_SIZE(set_10); ++i)
		is_double(set_10[i] * HKL_DEGTORAD, hkl_parameter_value_get(darray_item(g->axes, i), HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);

	/* check get USER unit */
	hkl_geometry_axis_values_get(g, values, 3, HKL_UNIT_USER);
	for(i=0; i<ARRAY_SIZE(set_10); ++i)
		is_double(set_10[i], values[i], HKL_EPSILON, __func__);

	hkl_geometry_free(g);
}

static void distance(void)
{
	int res = TRUE;
	HklGeometry *g1 = NULL;
	HklGeometry *g2 = NULL;
	HklHolder *holder = NULL;

	g1 = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(g1);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	g2 = hkl_geometry_new_copy(g1);

	res &= DIAG(hkl_geometry_set_values_v(g1, HKL_UNIT_DEFAULT, NULL, 0., 0., 0.));
	res &= DIAG(hkl_geometry_set_values_v(g2, HKL_UNIT_DEFAULT, NULL, 1., 1., 1.));
	is_double(3., hkl_geometry_distance(g1, g2), HKL_EPSILON, __func__);

	ok(res, __func__);

	hkl_geometry_free(g1);
	hkl_geometry_free(g2);
}

static void is_valid(void)
{
	int res = TRUE;
	HklGeometry *geom = NULL;
	HklHolder *holder = NULL;

	geom = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_DEFAULT, NULL, 0., 0., 0.));
	res &= DIAG(TRUE == hkl_geometry_is_valid(geom));

	res &= DIAG(hkl_geometry_set_values_v(geom, HKL_UNIT_DEFAULT, NULL, -181. * HKL_DEGTORAD, 0., 0.));
	res &= DIAG(TRUE == hkl_geometry_is_valid(geom));

	res &= DIAG(hkl_parameter_min_max_set(darray_item(geom->axes, 0),
					      -100 * HKL_DEGTORAD, 100 * HKL_DEGTORAD,
					      HKL_UNIT_DEFAULT, NULL));
	res &= DIAG(FALSE == hkl_geometry_is_valid(geom));

	ok(res, __func__);

	hkl_geometry_free(geom);
}

static void wavelength(void)
{
	HklGeometry *geom = NULL;
	GError *error;

	geom = hkl_geometry_new(NULL);

	is_double(1.54, hkl_geometry_wavelength_get(geom, HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);

	ok(TRUE == hkl_geometry_wavelength_set(geom, 2, HKL_UNIT_DEFAULT, NULL), __func__);
	is_double(2, hkl_geometry_wavelength_get(geom, HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);

	error = NULL;
	ok(TRUE == hkl_geometry_wavelength_set(geom, 2, HKL_UNIT_DEFAULT, &error), __func__);
	ok(error == NULL, __func__);
	is_double(2, hkl_geometry_wavelength_get(geom, HKL_UNIT_DEFAULT), HKL_EPSILON, __func__);

	hkl_geometry_free(geom);
}

static void list(void)
{
	int i = 0;
	int res = TRUE;
	HklGeometry *g;
	HklGeometryList *list;
	const HklGeometryListItem *item;
	HklHolder *holder;
	static double values[] = {0. * HKL_DEGTORAD, 10 * HKL_DEGTORAD, 30 * HKL_DEGTORAD};

	g = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	list = hkl_geometry_list_new();

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL, values[0], 0., 0.));
	hkl_geometry_list_add(list, g);
	is_int(1, hkl_geometry_list_n_items_get(list), __func__);

	/* can not add two times the same geometry */
	hkl_geometry_list_add(list, g);
	is_int(1, hkl_geometry_list_n_items_get(list), __func__);

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL, values[2], 0., 0.));
	hkl_geometry_list_add(list, g);
	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL, values[1], 0., 0.));
	hkl_geometry_list_add(list, g);
	is_int(3, hkl_geometry_list_n_items_get(list), __func__);

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL, values[0], 0., 0.));
	hkl_geometry_list_sort(list, g);

	HKL_GEOMETRY_LIST_FOREACH(item, list){
		is_double(values[i++],
			  hkl_parameter_value_get(darray_item(item->geometry->axes, 0), HKL_UNIT_DEFAULT),
			  HKL_EPSILON, __func__);
	}

	ok(res, __func__);

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

static void  list_multiply_from_range(void)
{
	int res = TRUE;
	HklGeometry *g;
	HklGeometryList *list;
	HklHolder *holder;
	HklParameter *axisA, *axisB, *axisC;

	g = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axisA = hkl_geometry_get_axis_by_name(g, "A");
	axisB = hkl_geometry_get_axis_by_name(g, "B");
	axisC = hkl_geometry_get_axis_by_name(g, "C");

	res &= DIAG(hkl_parameter_min_max_set(axisA, -190, 190, HKL_UNIT_USER, NULL));
	res &= DIAG(hkl_parameter_min_max_set(axisB, -190, 190, HKL_UNIT_USER, NULL));
	res &= DIAG(hkl_parameter_min_max_set(axisC, -190, 190, HKL_UNIT_USER, NULL));

	list = hkl_geometry_list_new();

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL,
					      185. * HKL_DEGTORAD, -185. * HKL_DEGTORAD, 190. * HKL_DEGTORAD));
	hkl_geometry_list_add(list, g);

	hkl_geometry_list_multiply_from_range(list);

	ok(res, __func__);

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

static void  list_remove_invalid(void)
{
	int res = TRUE;
	HklGeometry *g;
	HklGeometryList *list;
	HklHolder *holder;
	HklParameter *axisA, *axisB, *axisC;

	g = hkl_geometry_new(NULL);
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axisA = hkl_geometry_get_axis_by_name(g, "A");
	axisB = hkl_geometry_get_axis_by_name(g, "B");
	axisC = hkl_geometry_get_axis_by_name(g, "C");

	res &= DIAG(hkl_parameter_min_max_set(axisA, -100, 180., HKL_UNIT_USER, NULL));
	res &= DIAG(hkl_parameter_min_max_set(axisB, -100., 180., HKL_UNIT_USER, NULL));
	res &= DIAG(hkl_parameter_min_max_set(axisC, -100., 180., HKL_UNIT_USER, NULL));

	list = hkl_geometry_list_new();

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL,
					      185. * HKL_DEGTORAD,
					      -185.* HKL_DEGTORAD,
					      185. * HKL_DEGTORAD));
	hkl_geometry_list_add(list, g);

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL,
					      -190. * HKL_DEGTORAD,
					      -190.* HKL_DEGTORAD,
					      -190.* HKL_DEGTORAD));
	hkl_geometry_list_add(list, g);

	res &= DIAG(hkl_geometry_set_values_v(g, HKL_UNIT_DEFAULT, NULL,
					      180. * HKL_DEGTORAD,
					      180.* HKL_DEGTORAD,
					      180.* HKL_DEGTORAD));
	hkl_geometry_list_add(list, g);

	is_int(3, hkl_geometry_list_n_items_get(list), __func__);
	hkl_geometry_list_remove_invalid(list);
	is_int(2, hkl_geometry_list_n_items_get(list), __func__);

	ok(res, __func__);

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

int main(void)
{
	plan(50);

	add_holder();
	get_axis();
	update();
	set();
	axis_values_get_set();
	distance();
	is_valid();
	wavelength();

	list();
	list_multiply_from_range();
	list_remove_invalid();

	return 0;
}
