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

static void new(void)
{
	HklSample *sample;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_sample_free(sample);
}

static void add_reflection(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);

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

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);
	ref2 = hkl_sample_get_ith_reflection(sample, 0);
	ok(0 == !ref, __func__);
	ok(ref == ref2, __func__);
	is_int(1, sample->reflections_len, __func__);

	ref = hkl_sample_add_reflection(sample, geometry, detector, -1, 0, 0);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 1, 0);

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

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);
	hkl_sample_del_reflection(sample, 0);
	is_int(0, sample->reflections_len, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void  set_UB(void )
{
	HklSample *sample;
	static HklMatrix UB = {{{HKL_TAU/1.54,           0.,           0.},
				{          0.,           0., HKL_TAU/1.54},
				{          0.,-HKL_TAU/1.54,           0.}}};
	static HklMatrix U = {{{1., 0., 0.},
			       {0., 0., 1.},
			       {0.,-1., 0.}}};

	sample = hkl_sample_new("test",  HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_sample_set_UB(sample, &UB);
	ok(HKL_TRUE == hkl_matrix_cmp(&U, &sample->U), __func__);
	is_double(-90. * HKL_DEGTORAD, hkl_parameter_value_get(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
}

static void compute_UB_busing_levy(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 0, 1);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., -90., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, -1, 0, 0);

	hkl_sample_compute_UB_busing_levy(sample, 0, 1);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_I, &sample->U), __func__);
	is_double(0., hkl_parameter_value_get(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uz), HKL_EPSILON, __func__);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 90., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 180., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 1, 0);

	hkl_sample_compute_UB_busing_levy(sample, 2, 3);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_ref, &sample->U), __func__);
	is_double(-90. * HKL_DEGTORAD, hkl_parameter_value_get(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void affine(void)
{
	double a, b, c, alpha, beta, gamma;
	const HklFactory *factory;
	HklDetector *detector;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;
	static HklMatrix m_ref = {{{1., 0., 0.},
				   {0., 1., 0.},
				   {0., 0., 1.}}};

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_lattice_set(sample->lattice,
			1, 5, 4,
			92 * HKL_DEGTORAD,
			81 * HKL_DEGTORAD,
			90 * HKL_DEGTORAD);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 90., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 90., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 1, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 0, 1);

	hkl_geometry_set_values_unit_v(geometry, 60., 60., 60., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, .625, .75, -.216506350946);

	hkl_geometry_set_values_unit_v(geometry, 45., 45., 45., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, .665975615037, .683012701892, .299950211252);

	hkl_sample_affine(sample);

	a = hkl_parameter_value_get(sample->lattice->a);
	b = hkl_parameter_value_get(sample->lattice->b);
	c = hkl_parameter_value_get(sample->lattice->c);
	alpha = hkl_parameter_value_get(sample->lattice->alpha);
	beta = hkl_parameter_value_get(sample->lattice->beta);
	gamma = hkl_parameter_value_get(sample->lattice->gamma);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_ref, &sample->U), __func__);
	is_double(1.54, a, HKL_EPSILON, __func__);
	is_double(1.54, b, HKL_EPSILON, __func__);
	is_double(1.54, c, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, alpha, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, beta, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, gamma, HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
}

static void get_reflections_xxx_angle(void)
{
	HklDetector *detector;
	const HklFactory *factory;
	HklGeometry *geometry;
	HklSample *sample;
	HklSampleReflection *ref;

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       1.54, 1.54, 1.54,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 90., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 90., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 1, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 0, 1);

	hkl_geometry_set_values_unit_v(geometry, 60., 60., 60., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, .625, .75, -.216506350946);

	hkl_geometry_set_values_unit_v(geometry, 45., 45., 45., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, .665975615037, .683012701892, .299950211252);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_theoretical_angle(sample, 0, 1),
		  HKL_EPSILON, __func__);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_mesured_angle(sample, 0, 1),
		  HKL_EPSILON, __func__);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_theoretical_angle(sample, 1, 2),
		  HKL_EPSILON, __func__);

	is_double(90 * HKL_DEGTORAD,
		  hkl_sample_get_reflection_mesured_angle(sample, 1, 2),
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
	HklSampleReflection *ref;
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}};

	factory = hkl_factory_get_by_name("E4CV");
	geometry = hkl_factory_create_new_geometry(factory);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set(detector, 1);

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       1.54, 1.54, 1.54,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 90., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 1, 0, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 90., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 1, 0);

	hkl_geometry_set_values_unit_v(geometry, 30., 0., 0., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, 0, 0, 1);

	hkl_geometry_set_values_unit_v(geometry, 60., 60., 60., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, .625, .75, -.216506350946);

	hkl_geometry_set_values_unit_v(geometry, 46., 45., 45., 60.);
	ref = hkl_sample_add_reflection(sample, geometry, detector, .665975615037, .683012701892, .299950211252);

	/* correct the last reflection so the sample affinement must be ok. */
	hkl_geometry_set_values_unit_v(geometry, 45., 45., 45., 60.);
	hkl_sample_reflection_geometry_set(ref, geometry);

	hkl_sample_affine(sample);

	a = hkl_parameter_value_get(sample->lattice->a);
	b = hkl_parameter_value_get(sample->lattice->b);
	c = hkl_parameter_value_get(sample->lattice->c);
	alpha = hkl_parameter_value_get(sample->lattice->alpha);
	beta = hkl_parameter_value_get(sample->lattice->beta);
	gamma = hkl_parameter_value_get(sample->lattice->gamma);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_ref, &sample->U), __func__);
	is_double(1.54, a, HKL_EPSILON, __func__);
	is_double(1.54, b, HKL_EPSILON, __func__);
	is_double(1.54, c, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, alpha, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, beta, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, gamma, HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_value_get(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geometry);
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
