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
#include <hkl.h>
#include <tap/basic.h>
#include <tap/float.h>
#include "hkl-geometry-private.h"

static void add_holder(void)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;

	g = hkl_geometry_new();
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
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	HklAxis *axis0, *axis1, *axis2;

	g = hkl_geometry_new();

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	ok(0 == !hkl_geometry_get_axis_by_name(g, "A"), __func__);
	ok(0 == !hkl_geometry_get_axis_by_name(g, "B"), __func__);
	ok(0 == !hkl_geometry_get_axis_by_name(g, "C"), __func__);
	ok(1 == !hkl_geometry_get_axis_by_name(g, "D"), __func__);

	hkl_geometry_free(g);
}

static void update(void)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;
	HklAxis *axis0, *axis1, *axis2;

	g = hkl_geometry_new();

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axis1 = container_of(hkl_geometry_get_axis_by_name(g, "B"), HklAxis, parameter);
	hkl_parameter_set_value(&axis1->parameter, M_PI_2, NULL);
	/* now axis1 is dirty */
	ok(HKL_TRUE == axis1->parameter.changed, __func__);

	hkl_geometry_update(g);
	holder = darray_item(g->holders, 0);
	is_double(1./sqrt(2), holder->q.data[0], HKL_EPSILON, __func__);
	is_double(1./sqrt(2), holder->q.data[1], HKL_EPSILON, __func__);
	is_double(.0, holder->q.data[2], HKL_EPSILON, __func__);
	is_double(.0, holder->q.data[3], HKL_EPSILON, __func__);
	/* now axis1 is clean */
	ok(HKL_FALSE == axis1->parameter.changed, __func__);

	hkl_geometry_free(g);
}

static void set_values(void)
{
	HklGeometry *g;
	HklHolder *holder;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	hkl_geometry_set_values_v(g, 3, 1., 1., 1.);
	is_double(1., hkl_parameter_get_value(&darray_item(g->axes, 0)->parameter), HKL_EPSILON, __func__);
	is_double(1., hkl_parameter_get_value(&darray_item(g->axes, 1)->parameter), HKL_EPSILON, __func__);
	is_double(1., hkl_parameter_get_value(&darray_item(g->axes, 2)->parameter), HKL_EPSILON, __func__);

	hkl_geometry_free(g);
}

static void set_values_unit(void)
{
	HklGeometry *g;
	HklHolder *holder;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	hkl_geometry_set_values_unit_v(g, 10., 10., 10.);
	is_double(10. * HKL_DEGTORAD, hkl_parameter_get_value(&darray_item(g->axes, 0)->parameter), HKL_EPSILON, __func__);
	is_double(10. * HKL_DEGTORAD, hkl_parameter_get_value(&darray_item(g->axes, 1)->parameter), HKL_EPSILON, __func__);
	is_double(10. * HKL_DEGTORAD, hkl_parameter_get_value(&darray_item(g->axes, 2)->parameter), HKL_EPSILON, __func__);

	hkl_geometry_free(g);
}

static void distance(void)
{
	HklGeometry *g1 = NULL;
	HklGeometry *g2 = NULL;
	HklHolder *holder = NULL;

	g1 = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g1);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	g2 = hkl_geometry_new_copy(g1);

	hkl_geometry_set_values_v(g1, 3, 0., 0., 0.);
	hkl_geometry_set_values_v(g2, 3, 1., 1., 1.);
	is_double(3., hkl_geometry_distance(g1, g2), HKL_EPSILON, __func__);

	hkl_geometry_free(g1);
	hkl_geometry_free(g2);
}

static void is_valid(void)
{
	HklGeometry *geom = NULL;
	HklHolder *holder = NULL;

	geom = hkl_geometry_new();
	holder = hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	hkl_geometry_set_values_v(geom, 3, 0., 0., 0.);
	ok(HKL_TRUE == hkl_geometry_is_valid(geom), __func__);

	hkl_geometry_set_values_v(geom, 3, -181. * HKL_DEGTORAD, 0., 0.);
	ok(HKL_TRUE == hkl_geometry_is_valid(geom), __func__);

	hkl_parameter_set_range(&darray_item(geom->axes, 0)->parameter,
				-100 * HKL_DEGTORAD, 100 * HKL_DEGTORAD);
	ok(HKL_FALSE == hkl_geometry_is_valid(geom), __func__);

	hkl_geometry_free(geom);
}

static void list(void)
{
	int i = 0;
	HklGeometry *g;
	HklGeometryList *list;
	HklGeometryListItem **item;
	HklHolder *holder;
	static double values[] = {0. * HKL_DEGTORAD, 10 * HKL_DEGTORAD, 30 * HKL_DEGTORAD};

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	list = hkl_geometry_list_new();

	hkl_geometry_set_values_v(g, 3, values[0], 0., 0.);
	hkl_geometry_fprintf(stderr, g);
	hkl_geometry_list_add(list, g);
	is_int(1, darray_size(list->items), __func__);

	/* can not add two times the same geometry */
	hkl_geometry_list_add(list, g);
	is_int(1, darray_size(list->items), __func__);

	hkl_geometry_set_values_v(g, 3, values[2], 0., 0.);
	hkl_geometry_list_add(list, g);
	hkl_geometry_set_values_v(g, 3, values[1], 0., 0.);
	hkl_geometry_list_add(list, g);
	is_int(3, darray_size(list->items), __func__);

	hkl_geometry_set_values_v(g, 3, values[0], 0., 0.);
	hkl_geometry_list_sort(list, g);

	hkl_geometry_list_fprintf(stderr, list);
	darray_foreach(item, list->items){
		is_double(values[i++],
			  hkl_parameter_get_value(&darray_item((*item)->geometry->axes, 0)->parameter),
			  HKL_EPSILON, __func__);
	}

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

static void  list_multiply_from_range(void)
{
	HklGeometry *g;
	HklGeometryList *list;
	HklHolder *holder;
	HklParameter *axisA, *axisB, *axisC;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axisA = hkl_geometry_get_axis_by_name(g, "A");
	axisB = hkl_geometry_get_axis_by_name(g, "B");
	axisC = hkl_geometry_get_axis_by_name(g, "C");

	hkl_parameter_set_range_unit(axisA, -190, 190);
	hkl_parameter_set_range_unit(axisB, -190, 190);
	hkl_parameter_set_range_unit(axisC, -190, 190);

	list = hkl_geometry_list_new();

	hkl_geometry_set_values_v(g, 3, 185. * HKL_DEGTORAD, -185. * HKL_DEGTORAD, 190. * HKL_DEGTORAD);
	hkl_geometry_list_add(list, g);

	hkl_geometry_list_multiply_from_range(list);

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

static void  list_remove_invalid(void)
{
	HklGeometry *g;
	HklGeometryList *list;
	HklHolder *holder;
	HklParameter *axisA, *axisB, *axisC;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axisA = hkl_geometry_get_axis_by_name(g, "A");
	axisB = hkl_geometry_get_axis_by_name(g, "B");
	axisC = hkl_geometry_get_axis_by_name(g, "C");

	hkl_parameter_set_range_unit(axisA, -100, 180.);
	hkl_parameter_set_range_unit(axisB, -100., 180.);
	hkl_parameter_set_range_unit(axisC, -100., 180.);

	list = hkl_geometry_list_new();

	hkl_geometry_set_values_v(g, 3,
				  185. * HKL_DEGTORAD,
				  -185.* HKL_DEGTORAD,
				  185. * HKL_DEGTORAD);
	hkl_geometry_list_add(list, g);

	hkl_geometry_set_values_v(g, 3,
				  -190. * HKL_DEGTORAD,
				  -190.* HKL_DEGTORAD,
				  -190.* HKL_DEGTORAD);
	hkl_geometry_list_add(list, g);

	hkl_geometry_set_values_v(g, 3,
				  180. * HKL_DEGTORAD,
				  180.* HKL_DEGTORAD,
				  180.* HKL_DEGTORAD);
	hkl_geometry_list_add(list, g);

	is_int(3, darray_size(list->items), __func__);
	hkl_geometry_list_remove_invalid(list);
	is_int(2, darray_size(list->items), __func__);

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

int main(int argc, char** argv)
{
	plan(32);

	add_holder();
	get_axis();
	update();
	set_values();
	set_values_unit();
	distance();
	is_valid();

	list();
	list_multiply_from_range();
	list_remove_invalid();

	return 0;
}
