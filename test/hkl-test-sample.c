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

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_sample_free(sample);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(add_reflection)
{
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_reflection)
{
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklSampleReflection *ref2;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);
	ref2 = hkl_sample_get_ith_reflection(sample, 0);
	HKL_ASSERT_EQUAL(0, !ref);
	HKL_ASSERT_POINTER_EQUAL(ref, ref2);
	HKL_ASSERT_EQUAL(1, HKL_LIST_LEN(sample->reflections));

	ref = hkl_sample_add_reflection(sample, geom, detector, -1, 0, 0);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(del_reflection)
{
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);
	hkl_sample_del_reflection(sample, 0);
	HKL_ASSERT_EQUAL(0, HKL_LIST_LEN(sample->reflections));

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(compute_UB_busing_levy)
{
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 0, 1);

	SET_ANGLES(geom, 30, 0, -90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, -1, 0, 0);

	hkl_sample_compute_UB_busing_levy(sample, 0, 1);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_I, &sample->U));

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	SET_ANGLES(geom, 30, 0, 180, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	hkl_sample_compute_UB_busing_levy(sample, 2, 3);
	HKL_ASSERT_EQUAL(HKL_TRUE, hkl_matrix_cmp(&m_ref, &sample->U));

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(affine)
{
	double a, b, c, alpha, beta, gamma;
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	sample->lattice->a->value = 1;
	sample->lattice->b->value = 5;
	sample->lattice->c->value = 4;
	sample->lattice->alpha->value = 92 * HKL_DEGTORAD;
	sample->lattice->beta->value = 81 * HKL_DEGTORAD;
	sample->lattice->gamma->value = 90 * HKL_DEGTORAD;

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	SET_ANGLES(geom, 30, 90, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 0, 1);

	SET_ANGLES(geom, 60, 60, 60, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, .625, .75, -.216506350946);

	SET_ANGLES(geom, 45, 45, 45, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, .665975615037, .683012701892, .299950211252);

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
	hkl_detector_free(detector);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(get_reflections_xxx_angle)
{
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       1.54, 1.54, 1.54,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD);

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	SET_ANGLES(geom, 30, 90, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 0, 1);

	SET_ANGLES(geom, 60, 60, 60, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, .625, .75, -.216506350946);

	SET_ANGLES(geom, 45, 45, 45, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, .665975615037, .683012701892, .299950211252);

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
	hkl_detector_free(detector);
	hkl_geometry_free(geom);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(reflection_set_geometry)
{
	double a, b, c, alpha, beta, gamma;
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}};

	geom = hkl_geometry_factory_new(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_set_lattice(sample,
			       1.54, 1.54, 1.54,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD,90*HKL_DEGTORAD);

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	SET_ANGLES(geom, 30, 90, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 0, 1);

	SET_ANGLES(geom, 60, 60, 60, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, .625, .75, -.216506350946);

	SET_ANGLES(geom, 46, 45, 45, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, .665975615037, .683012701892, .299950211252);

	// correct the last reflection so the sample affinement must be ok.
	SET_ANGLES(geom, 45, 45, 45, 60);
	hkl_sample_reflection_set_geometry(ref, geom);

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
	hkl_detector_free(detector);
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
	sample1 = hkl_sample_new("test1", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	sample2 = hkl_sample_new("test2", HKL_SAMPLE_TYPE_MONOCRYSTAL);

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
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_sample_list_append(samples, sample);

	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_sample_list_select_current(samples, "test"));
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_sample_list_select_current(samples, "tests"));


	hkl_sample_list_free(samples); // also release sample

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(list_clear)
{
	size_t i;
	HklSampleList *samples;
	HklSample *sample1;
	HklSample *sample2;

	samples = hkl_sample_list_new();
	for(i=0; i<2; ++i){ // two times to see if the clear has no side effect
		sample1 = hkl_sample_new("test1", HKL_SAMPLE_TYPE_MONOCRYSTAL);
		sample2 = hkl_sample_new("test2", HKL_SAMPLE_TYPE_MONOCRYSTAL);

		hkl_sample_list_append(samples, sample1);
		hkl_sample_list_append(samples, sample2);
		hkl_sample_list_clear(samples);

		HKL_ASSERT_EQUAL(0, hkl_sample_list_len(samples));
	}

	hkl_sample_list_free(samples); // also relase sample1 and sample2

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

HKL_TEST( reflection_set_geometry );

HKL_TEST( list_new );
HKL_TEST( list_append_sample );
HKL_TEST( list_select_current );
HKL_TEST( list_clear );

HKL_TEST_SUITE_END

#undef SET_ANGLES
