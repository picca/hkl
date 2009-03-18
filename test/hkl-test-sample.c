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

#include <hkl/hkl-sample.h>
#include <hkl/hkl-geometry-factory.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME sample

#define SET_ANGLES(geom, a, b, c, d) hkl_geometry_set_values_v(geom, 4,	\
							      (a) * HKL_DEGTORAD, \
							      (b) * HKL_DEGTORAD, \
							      (c) * HKL_DEGTORAD, \
							      (d) * HKL_DEGTORAD)
HKL_TEST_SUITE_FUNC(new)
{
	HklSample *sample;

	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	hkl_sample_free(sample);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(add_reflection)
{
	HklDetector det = {1};
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, &det, 1, 0, 0);

	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_reflection)
{
	HklDetector det = {1};
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklSampleReflection *ref2;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, &det, 1, 0, 0);
	ref2 = hkl_sample_get_ith_reflection(sample, 0);
	HKL_ASSERT_POINTER_EQUAL(ref, ref2);

	ref = hkl_sample_add_reflection(sample, geom, &det, -1, 0, 0);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 1, 0);

	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(del_reflection)
{
	HklDetector det = {1};
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, &det, 1, 0, 0);
	hkl_sample_del_reflection(sample, 0);
	HKL_ASSERT_EQUAL(0, HKL_LIST_LEN(sample->reflections));

	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(compute_UB_busing_levy)
{
	HklDetector det = {1};
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);

	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 0, 1);

	SET_ANGLES(geom, 30, 0, -90, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, -1, 0, 0);

	hkl_sample_compute_UB_busing_levy(sample, 0, 1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_I, &sample->U));

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 1, 0, 0);

	SET_ANGLES(geom, 30, 0, 180, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 1, 0);

	hkl_sample_compute_UB_busing_levy(sample, 2, 3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &sample->U));

	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(affine)
{
	double a, b, c, alpha, beta, gamma;
	HklDetector det = {1};
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);

	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	sample->lattice->a->value = 1;
	sample->lattice->b->value = 5;
	sample->lattice->c->value = 4;
	sample->lattice->alpha->value = 92 * HKL_DEGTORAD;
	sample->lattice->beta->value = 81 * HKL_DEGTORAD;
	sample->lattice->gamma->value = 90 * HKL_DEGTORAD;

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 1, 0, 0);

	SET_ANGLES(geom, 30, 90, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 1, 0);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 0, 1);

	SET_ANGLES(geom, 60, 60, 60, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, .625, .75, -.216506350946);

	SET_ANGLES(geom, 45, 45, 45, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, .665975615037, .683012701892, .299950211252);

	hkl_sample_affine(sample);

	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &sample->U));
	HKL_ASSERT_DOUBLES_EQUAL(1.54, a, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, b, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1.54, c, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, alpha, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, beta, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, gamma, HKL_EPSILON);

	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_reflections_xxx_angle)
{
	HklDetector det = {1};
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_EULERIAN4C_VERTICAL);

	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       1.54, 1.54, 1.54,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD);

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 1, 0, 0);

	SET_ANGLES(geom, 30, 90, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 1, 0);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, 0, 0, 1);

	SET_ANGLES(geom, 60, 60, 60, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, .625, .75, -.216506350946);

	SET_ANGLES(geom, 45, 45, 45, 60);
	ref = hkl_sample_add_reflection(sample, geom, &det, .665975615037, .683012701892, .299950211252);

	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD,
				 hkl_sample_get_reflection_theoretical_angle(sample, 0, 1),
				 HKL_EPSILON);

	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD,
				 hkl_sample_get_reflection_mesured_angle(sample, 0, 1),
				 HKL_EPSILON);

	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD,
				 hkl_sample_get_reflection_theoretical_angle(sample, 1, 2),
				 HKL_EPSILON);

	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD,
				 hkl_sample_get_reflection_mesured_angle(sample, 1, 2),
				 HKL_EPSILON);

	hkl_sample_free(sample);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(list_new)
{
	HklSampleList *samples;

	samples = hkl_sample_list_new();

	hkl_sample_list_free(samples);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(list_append_sample)
{
	HklSampleList *samples;
	HklSample *sample1;
	HklSample *sample2;

	samples = hkl_sample_list_new();
	sample1 = hkl_sample_new("test1", HKL_SAMPLE_MONOCRYSTAL);
	sample2 = hkl_sample_new("test2", HKL_SAMPLE_MONOCRYSTAL);

	HKL_ASSERT_POINTER_EQUAL(sample1, hkl_sample_list_append(samples, sample1));
	HKL_ASSERT_EQUAL(0, hkl_sample_list_get_idx_from_name(samples, "test1"));

	HKL_ASSERT_POINTER_EQUAL(sample2, hkl_sample_list_append(samples, sample2));
	HKL_ASSERT_EQUAL(0, hkl_sample_list_get_idx_from_name(samples, "test1"));
	HKL_ASSERT_EQUAL(1, hkl_sample_list_get_idx_from_name(samples, "test2"));

	// can not have two samples with the same name.
	HKL_ASSERT_POINTER_EQUAL(NULL, hkl_sample_list_append(samples, sample1));

	hkl_sample_list_free(samples); // also relase sample1 and sample2

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(list_select_current)
{
	HklSampleList *samples;
	HklSample *sample;

	samples = hkl_sample_list_new();
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);

	hkl_sample_list_append(samples, sample);

	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_sample_list_select_current(samples, "test"));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_sample_list_select_current(samples, "tests"));


	hkl_sample_list_free(samples); // also release sample

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );
HKL_TEST( add_reflection );
HKL_TEST( get_reflection );
HKL_TEST( del_reflection );
HKL_TEST( compute_UB_busing_levy );
HKL_TEST( affine );
HKL_TEST( get_reflections_xxx_angle );

HKL_TEST( list_new );
HKL_TEST( list_append_sample );
HKL_TEST( list_select_current );

HKL_TEST_SUITE_END

#undef SET_ANGLES
