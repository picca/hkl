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
#include <tap/basic.h>

static void add_holder(void)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;

	g = hkl_geometry_new();
	is_int(0, g->holders_len, __func__);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	is_int(1, g->holders_len, __func__);

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);
	is_int(2, g->holders_len, __func__);

	ok(holder == &g->holders[1], __func__);

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

	axis1 = hkl_geometry_get_axis_by_name(g, "B");
	hkl_axis_set_value(axis1, M_PI_2);
	/* now axis1 is dirty */
	ok(HKL_TRUE == hkl_axis_get_changed(axis1), __func__);

	hkl_geometry_update(g);
	is_double(1./sqrt(2), g->holders[0].q.data[0], HKL_EPSILON, __func__);
	is_double(1./sqrt(2), g->holders[0].q.data[1], HKL_EPSILON, __func__);
	is_double(.0, g->holders[0].q.data[2], HKL_EPSILON, __func__);
	is_double(.0, g->holders[0].q.data[3], HKL_EPSILON, __func__);
	/* now axis1 is clean */
	ok(HKL_FALSE == hkl_axis_get_changed(axis1), __func__);

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
	is_double(1., hkl_axis_get_value(&g->axes[0]), HKL_EPSILON, __func__);
	is_double(1., hkl_axis_get_value(&g->axes[1]), HKL_EPSILON, __func__);
	is_double(1., hkl_axis_get_value(&g->axes[2]), HKL_EPSILON, __func__);

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
	is_double(10. * HKL_DEGTORAD, hkl_axis_get_value(&g->axes[0]), HKL_EPSILON, __func__);
	is_double(10. * HKL_DEGTORAD, hkl_axis_get_value(&g->axes[1]), HKL_EPSILON, __func__);
	is_double(10. * HKL_DEGTORAD, hkl_axis_get_value(&g->axes[2]), HKL_EPSILON, __func__);

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

	hkl_geometry_set_values_v(geom, 3, -180., 0., 0.);
	ok(HKL_FALSE == hkl_geometry_is_valid(geom), __func__);

	hkl_geometry_free(geom);
}

static void list(void)
{
	HklGeometry *g;
	HklGeometryList *list;
	HklHolder *holder;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	list = hkl_geometry_list_new();

	hkl_geometry_set_values_v(g, 3, 0., 0., 0.);
	hkl_geometry_list_add(list, g);
	is_int(1, list->len, __func__);

	/* can not add two times the same geometry */
	hkl_geometry_list_add(list, g);
	is_int(1, list->len, __func__);

	hkl_geometry_set_values_v(g, 3, 30*HKL_DEGTORAD, 0., 0.);
	hkl_geometry_list_add(list, g);
	hkl_geometry_set_values_v(g, 3, 10*HKL_DEGTORAD, 0., 0.);
	hkl_geometry_list_add(list, g);
	is_int(3, list->len, __func__);

	hkl_geometry_set_values_v(g, 3, 0., 0., 0.);
	hkl_geometry_list_sort(list, g);
	is_double(0.,
		  hkl_axis_get_value(&list->items[0].geometry->axes[0]),
		  HKL_EPSILON, __func__);
	is_double(10*HKL_DEGTORAD,
		  hkl_axis_get_value(&list->items[1].geometry->axes[0]),
		  HKL_EPSILON, __func__);
	is_double(30*HKL_DEGTORAD,
		  hkl_axis_get_value(&list->items[2].geometry->axes[0]),
		  HKL_EPSILON, __func__);


	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

static void  list_multiply_from_range(void)
{
	HklGeometry *g;
	HklGeometryList *list;
	HklHolder *holder;
	HklAxis *axisA, *axisB, *axisC;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axisA = hkl_geometry_get_axis_by_name(g, "A");
	axisB = hkl_geometry_get_axis_by_name(g, "B");
	axisC = hkl_geometry_get_axis_by_name(g, "C");

	hkl_axis_set_range_unit(axisA, -190, 190);
	hkl_axis_set_range_unit(axisB, -190, 190);
	hkl_axis_set_range_unit(axisC, -190, 190);

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
	HklAxis *axisA, *axisB, *axisC;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	axisA = hkl_geometry_get_axis_by_name(g, "A");
	axisB = hkl_geometry_get_axis_by_name(g, "B");
	axisC = hkl_geometry_get_axis_by_name(g, "C");

	hkl_axis_set_range_unit(axisA, -190., 180.);
	hkl_axis_set_range_unit(axisB, -190., 180.);
	hkl_axis_set_range_unit(axisC, -190., 180.);

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

	is_int(3, list->len, __func__);
	hkl_geometry_list_remove_invalid(list);
	is_int(2, list->len, __func__);

	hkl_geometry_free(g);
	hkl_geometry_list_free(list);
}

int main(int argc, char** argv)
{
	plan(31);

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
