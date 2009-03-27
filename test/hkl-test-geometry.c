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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <math.h>

#include <hkl/hkl-geometry.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME geometry

HKL_TEST_SUITE_FUNC(add_holder)
{
	HklGeometry *g = NULL;
	HklHolder *holder = NULL;

	g = hkl_geometry_new();
	HKL_ASSERT_EQUAL(0, HKL_LIST_LEN(g->holders));

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	HKL_ASSERT_EQUAL(1, HKL_LIST_LEN(g->holders));

	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);
	HKL_ASSERT_EQUAL(2, HKL_LIST_LEN(g->holders));

	HKL_ASSERT_POINTER_EQUAL(holder, &g->holders[1]);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_axis)
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

	HKL_ASSERT_EQUAL(0, !hkl_geometry_get_axis_by_name(g, "A"));
	HKL_ASSERT_EQUAL(0, !hkl_geometry_get_axis_by_name(g, "B"));
	HKL_ASSERT_EQUAL(0, !hkl_geometry_get_axis_by_name(g, "C"));
	HKL_ASSERT_EQUAL(1, !hkl_geometry_get_axis_by_name(g, "D"));

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(update)
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
	// now axis1 is dirty
	HKL_ASSERT_EQUAL(HKL_TRUE, axis1->parent.changed);
	
	hkl_geometry_update(g);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2), g->holders[0].q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2), g->holders[0].q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, g->holders[0].q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, g->holders[0].q.data[3], HKL_EPSILON);
	// now axis1 is clean
	HKL_ASSERT_EQUAL(HKL_FALSE, axis1->parent.changed);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(set_values)
{
	HklGeometry *g;
	HklHolder *holder;

	g = hkl_geometry_new();
	holder = hkl_geometry_add_holder(g);
	hkl_holder_add_rotation_axis(holder, "A", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "B", 1., 0., 0.);
	hkl_holder_add_rotation_axis(holder, "C", 1., 0., 0.);

	hkl_geometry_set_values_v(g, 3, 1., 1., 1.);
	HKL_ASSERT_DOUBLES_EQUAL(1., g->axes[0].parent.value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., g->axes[1].parent.value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., g->axes[2].parent.value, HKL_EPSILON);

	hkl_geometry_free(g);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(distance)
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
	HKL_ASSERT_DOUBLES_EQUAL(3, hkl_geometry_distance(g1, g2), HKL_EPSILON);

	hkl_geometry_free(g1);
	hkl_geometry_free(g2);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(list)
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
	HKL_ASSERT_EQUAL(1, HKL_LIST_LEN(list->geometries));

	// can not add two times the same geometry
	hkl_geometry_list_add(list, g);
	HKL_ASSERT_EQUAL(1, HKL_LIST_LEN(list->geometries));

	hkl_geometry_set_values_v(g, 3, 30*HKL_DEGTORAD, 0., 0.);
	hkl_geometry_list_add(list, g);
	hkl_geometry_set_values_v(g, 3, 10*HKL_DEGTORAD, 0., 0.);
	hkl_geometry_list_add(list, g);
	HKL_ASSERT_EQUAL(3, HKL_LIST_LEN(list->geometries));

	hkl_geometry_set_values_v(g, 3, 0., 0., 0.);
	hkl_geometry_list_sort(list, g);
	HKL_ASSERT_DOUBLES_EQUAL(0., list->geometries[0]->axes[0].parent.value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(10*HKL_DEGTORAD, list->geometries[1]->axes[0].parent.value, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(30*HKL_DEGTORAD, list->geometries[2]->axes[0].parent.value, HKL_EPSILON);


	hkl_geometry_free(g);
	hkl_geometry_list_free(list);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( add_holder );
HKL_TEST( get_axis );
HKL_TEST( update );
HKL_TEST( set_values );
HKL_TEST( distance );

HKL_TEST( list );

HKL_TEST_SUITE_END
