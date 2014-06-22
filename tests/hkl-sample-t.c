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
#include "hkl.h"
#include <tap/basic.h>
#include <tap/float.h>

static void new(void)
{
	HklSample *sample;

	sample = hkl_sample_new("test");

	hkl_sample_free(sample);
}

static void add_reflection(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");

	ref = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, ref);

	/* we can not add two times the same reflection */
	hkl_sample_add_reflection(sample, ref);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void get_reflection(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;
	HklSampleReflection *ref2;

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");

	ref = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, ref);
	ref2 = hkl_sample_first_reflection_get(sample);
	ok(0 == !ref, __func__);
	ok(ref == ref2, __func__);
	ok(NULL == hkl_sample_next_reflection_get(sample, ref2), __func__);

	ref = hkl_sample_reflection_new(geometry, detector, -1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, ref);
	ref = hkl_sample_reflection_new(geometry, detector, 0, 1, 0, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void del_reflection(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");

	ref = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, ref);
	hkl_sample_del_reflection(sample, ref);
	ok (NULL == hkl_sample_first_reflection_get(sample), __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void  set_UB(void )
{
	HklSample *sample;
	HklMatrix *UB = hkl_matrix_new_full(HKL_TAU/1.54, 0., 0.,
					    0., 0., HKL_TAU/1.54,
					    0., -HKL_TAU/1.54, 0.);
	HklMatrix *U = hkl_matrix_new_full(1., 0., 0.,
					   0., 0., 1.,
					   0.,-1., 0.);

	sample = hkl_sample_new("test");

	hkl_sample_UB_set(sample, UB, NULL);
	ok(TRUE == hkl_matrix_cmp(U,
				      hkl_sample_U_get(sample)), __func__);
	is_double(-90. * HKL_DEGTORAD,
		  hkl_parameter_value_get(hkl_sample_ux_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uy_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uz_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_matrix_free(U);
	hkl_matrix_free(UB);
}

static void compute_UB_busing_levy(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *r0, *r1, *r2, *r3;
	HklMatrix *m_I = hkl_matrix_new_full(1,0,0,
					     0,1,0,
					     0, 0, 1);
	HklMatrix *m_ref = hkl_matrix_new_full(1., 0., 0.,
					       0., 0., 1.,
					       0.,-1., 0.);

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.);
	r0 = hkl_sample_reflection_new(geometry, detector, 0, 0, 1, NULL);
	hkl_sample_add_reflection(sample, r0);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., -90., 60.);
	r1 = hkl_sample_reflection_new(geometry, detector, -1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, r1);

	hkl_sample_compute_UB_busing_levy(sample, r0, r1, NULL);
	ok(TRUE == hkl_matrix_cmp(m_I, hkl_sample_U_get(sample)), __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_ux_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uy_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uz_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 90., 60.);
	r2 = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, r2);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 180., 60.);
	r3 = hkl_sample_reflection_new(geometry, detector, 0, 1, 0, NULL);
	hkl_sample_add_reflection(sample, r3);

	hkl_sample_compute_UB_busing_levy(sample, r2, r3, NULL);
	ok(TRUE == hkl_matrix_cmp(m_ref, hkl_sample_U_get(sample)), __func__);
	is_double(-90. * HKL_DEGTORAD,
		  hkl_parameter_value_get(hkl_sample_ux_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uy_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uz_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
	hkl_matrix_free(m_ref);
	hkl_matrix_free(m_I);
}

static void affine(void)
{
	double a, b, c, alpha, beta, gamma;
	const HklFactory *factory;
	HklDetector *detector;
	HklGeometry *geometry;
	HklSample *sample;
	HklLattice *lattice;
	HklSampleReflection *ref;
	HklMatrix *m_ref = hkl_matrix_new_full(1., 0., 0.,
					       0., 1., 0.,
					       0., 0., 1.);

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(1, 5, 4,
				  92 * HKL_DEGTORAD,
				  81 * HKL_DEGTORAD,
				  90 * HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 90., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 90., 0., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, 0, 1, 0, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, 0, 0, 1, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 60., 60., 60., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, .625, .75, -.216506350946, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 45., 45., 45., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, .665975615037, .683012701892, .299950211252, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_sample_affine(sample, NULL);

	hkl_lattice_get(hkl_sample_lattice_get(sample),
			&a, &b, &c, &alpha, &beta, &gamma);

	ok(TRUE == hkl_matrix_cmp(m_ref, hkl_sample_U_get(sample)), __func__);
	is_double(1.54, a, HKL_EPSILON, __func__);
	is_double(1.54, b, HKL_EPSILON, __func__);
	is_double(1.54, c, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, alpha, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, beta, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, gamma, HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_ux_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uy_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uz_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
	hkl_matrix_free(m_ref);
}

static void get_reflections_xxx_angle(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklLattice *lattice;
	HklSampleReflection *r0, *r1, *r2, *r3, *r4;

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 90., 60.);
	r0 = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, r0);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 90., 0., 60.);
	r1 = hkl_sample_reflection_new(geometry, detector, 0, 1, 0, NULL);
	hkl_sample_add_reflection(sample, r1);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.);
	r2 = hkl_sample_reflection_new(geometry, detector, 0, 0, 1, NULL);
	hkl_sample_add_reflection(sample, r2);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 60., 60., 60., 60.);
	r3 = hkl_sample_reflection_new(geometry, detector, .625, .75, -.216506350946, NULL);
	hkl_sample_add_reflection(sample, r3);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 45., 45., 45., 60.);
	r4 = hkl_sample_reflection_new(geometry, detector, .665975615037, .683012701892, .299950211252, NULL);
	hkl_sample_add_reflection(sample, r4);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_theoretical_angle(sample, r0, r1),
		  HKL_EPSILON, __func__);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_mesured_angle(sample, r0, r1),
		  HKL_EPSILON, __func__);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_theoretical_angle(sample, r1, r2),
		  HKL_EPSILON, __func__);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_mesured_angle(sample, r1, r2),
		  HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void reflection_set_geometry(void)
{
	double a, b, c, alpha, beta, gamma;
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklLattice *lattice;
	HklSampleReflection *ref;
	HklMatrix *m_ref = hkl_matrix_new_full(1., 0., 0.,
					       0., 1., 0.,
					       0., 0., 1.);

	factory = hkl_factory_get_by_name("E4CV", NULL);
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test");
	lattice = hkl_lattice_new(1.54, 1.54, 1.54,
				  90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD,
				  NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 90., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, 1, 0, 0, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 90., 0., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, 0, 1, 0, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 30., 0., 0., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, 0, 0, 1, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 60., 60., 60., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, .625, .75, -.216506350946, NULL);
	hkl_sample_add_reflection(sample, ref);

	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 46., 45., 45., 60.);
	ref = hkl_sample_reflection_new(geometry, detector, .665975615037, .683012701892, .299950211252, NULL);
	hkl_sample_add_reflection(sample, ref);

	/* correct the last reflection so the sample affinement must be ok. */
	hkl_geometry_set_values_v(geometry, HKL_UNIT_USER, NULL, 45., 45., 45., 60.);
	hkl_sample_reflection_geometry_set(ref, geometry);

	hkl_sample_affine(sample, NULL);
	hkl_lattice_get(hkl_sample_lattice_get(sample),
			&a, &b, &c, &alpha, &beta, &gamma);

	ok(TRUE == hkl_matrix_cmp(m_ref, hkl_sample_U_get(sample)), __func__);
	is_double(1.54, a, HKL_EPSILON, __func__);
	is_double(1.54, b, HKL_EPSILON, __func__);
	is_double(1.54, c, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, alpha, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, beta, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, gamma, HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_ux_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uy_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);
	is_double(0.,
		  hkl_parameter_value_get(hkl_sample_uz_get(sample), HKL_UNIT_DEFAULT),
		  HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
	hkl_matrix_free(m_ref);
}

int main(int argc, char** argv)
{
	plan(40);

	new();
	add_reflection();
	get_reflection();
	del_reflection();
	set_UB();
	compute_UB_busing_levy();
	affine();
	get_reflections_xxx_angle();

	reflection_set_geometry();

	return 0;
}
