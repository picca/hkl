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

#define SET_ANGLES(geom, a, b, c, d) hkl_geometry_set_values_v(geom, 4,	\
							       (a) * HKL_DEGTORAD, \
							       (b) * HKL_DEGTORAD, \
							       (c) * HKL_DEGTORAD, \
							       (d) * HKL_DEGTORAD)
static void new(void)
{
	HklSample *sample;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_sample_free(sample);
}

static void add_reflection(void)
{
	HklDetector *detector;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void get_reflection(void)
{
	HklDetector *detector;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklSampleReflection *ref2;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);
	ref2 = hkl_sample_get_ith_reflection(sample, 0);
	ok(0 == !ref, __func__);
	ok(ref == ref2, __func__);
	is_int(1, sample->reflections_len, __func__);

	ref = hkl_sample_add_reflection(sample, geom, detector, -1, 0, 0);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void del_reflection(void)
{
	HklDetector *detector;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);
	hkl_sample_del_reflection(sample, 0);
	is_int(0, sample->reflections_len, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
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
	is_double(-90. * HKL_DEGTORAD, hkl_parameter_get_value(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
}

static void compute_UB_busing_levy(void)
{
	HklDetector *detector;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	SET_ANGLES(geom, 30, 0, 0, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 0, 1);

	SET_ANGLES(geom, 30, 0, -90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, -1, 0, 0);

	hkl_sample_compute_UB_busing_levy(sample, 0, 1);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_I, &sample->U), __func__);
	is_double(0., hkl_parameter_get_value(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uz), HKL_EPSILON, __func__);

	SET_ANGLES(geom, 30, 0, 90, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 1, 0, 0);

	SET_ANGLES(geom, 30, 0, 180, 60);
	ref = hkl_sample_add_reflection(sample, geom, detector, 0, 1, 0);

	hkl_sample_compute_UB_busing_levy(sample, 2, 3);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_ref, &sample->U), __func__);
	is_double(-90. * HKL_DEGTORAD, hkl_parameter_get_value(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void affine(void)
{
	double a, b, c, alpha, beta, gamma;
	const HklGeometryConfig *config;
	HklDetector *detector;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	static HklMatrix m_ref = {{{1., 0., 0.},
				   {0., 1., 0.},
				   {0., 0., 1.}}};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

	detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
	detector->idx = 1;

	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_lattice_set(sample->lattice,
			1, 5, 4,
			92 * HKL_DEGTORAD,
			81 * HKL_DEGTORAD,
			90 * HKL_DEGTORAD);

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

	a = hkl_parameter_get_value(sample->lattice->a);
	b = hkl_parameter_get_value(sample->lattice->b);
	c = hkl_parameter_get_value(sample->lattice->c);
	alpha = hkl_parameter_get_value(sample->lattice->alpha);
	beta = hkl_parameter_get_value(sample->lattice->beta);
	gamma = hkl_parameter_get_value(sample->lattice->gamma);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_ref, &sample->U), __func__);
	is_double(1.54, a, HKL_EPSILON, __func__);
	is_double(1.54, b, HKL_EPSILON, __func__);
	is_double(1.54, c, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, alpha, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, beta, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, gamma, HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void get_reflections_xxx_angle(void)
{
	HklDetector *detector;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

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
	hkl_geometry_free(geom);
}

static void reflection_set_geometry(void)
{
	double a, b, c, alpha, beta, gamma;
	HklDetector *detector;
	const HklGeometryConfig *config;
	HklGeometry *geom;
	HklSample *sample;
	HklSampleReflection *ref;
	HklMatrix m_ref = {{{1., 0., 0.}, {0., 1., 0.}, {0., 0., 1.}}};

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
	geom = hkl_geometry_factory_new(config);

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

	/* correct the last reflection so the sample affinement must be ok. */
	SET_ANGLES(geom, 45, 45, 45, 60);
	hkl_sample_reflection_set_geometry(ref, geom);

	hkl_sample_affine(sample);

	a = hkl_parameter_get_value(sample->lattice->a);
	b = hkl_parameter_get_value(sample->lattice->b);
	c = hkl_parameter_get_value(sample->lattice->c);
	alpha = hkl_parameter_get_value(sample->lattice->alpha);
	beta = hkl_parameter_get_value(sample->lattice->beta);
	gamma = hkl_parameter_get_value(sample->lattice->gamma);
	ok(HKL_TRUE == hkl_matrix_cmp(&m_ref, &sample->U), __func__);
	is_double(1.54, a, HKL_EPSILON, __func__);
	is_double(1.54, b, HKL_EPSILON, __func__);
	is_double(1.54, c, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, alpha, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, beta, HKL_EPSILON, __func__);
	is_double(90 * HKL_DEGTORAD, gamma, HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->ux), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uy), HKL_EPSILON, __func__);
	is_double(0., hkl_parameter_get_value(sample->uz), HKL_EPSILON, __func__);

	hkl_sample_free(sample);
	hkl_detector_free(detector);
	hkl_geometry_free(geom);
}

static void list_new(void)
{
	HklSampleList *samples;

	samples = hkl_sample_list_new();

	hkl_sample_list_free(samples);
}

static void list_append_sample(void)
{
	HklSampleList *samples;
	HklSample *sample1;
	HklSample *sample2;

	samples = hkl_sample_list_new();
	sample1 = hkl_sample_new("test1", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	sample2 = hkl_sample_new("test2", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	ok(sample1 == hkl_sample_list_append(samples, sample1), __func__);
	is_int(0, hkl_sample_list_get_idx_from_name(samples, "test1"), __func__);

	ok(sample2 == hkl_sample_list_append(samples, sample2), __func__);
	is_int(0, hkl_sample_list_get_idx_from_name(samples, "test1"), __func__);
	is_int(1, hkl_sample_list_get_idx_from_name(samples, "test2"), __func__);

	/* can not have two samples with the same name. */
	ok(NULL == hkl_sample_list_append(samples, sample1), __func__);

	/* also relase sample1 and sample2 */
	hkl_sample_list_free(samples);
}

static void list_select_current(void)
{
	HklSampleList *samples;
	HklSample *sample;

	samples = hkl_sample_list_new();
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	hkl_sample_list_append(samples, sample);

	ok(HKL_TRUE == hkl_sample_list_select_current(samples, "test"), __func__);
	ok(HKL_FALSE == hkl_sample_list_select_current(samples, "tests"), __func__);

	/* also relase sample */
	hkl_sample_list_free(samples);
}

static void list_clear(void)
{
	size_t i;
	HklSampleList *samples;
	HklSample *sample1;
	HklSample *sample2;

	samples = hkl_sample_list_new();
	for(i=0; i<2; ++i){
		/* two times to see if the clear has no side effect */
		sample1 = hkl_sample_new("test1", HKL_SAMPLE_TYPE_MONOCRYSTAL);
		sample2 = hkl_sample_new("test2", HKL_SAMPLE_TYPE_MONOCRYSTAL);

		hkl_sample_list_append(samples, sample1);
		hkl_sample_list_append(samples, sample2);
		hkl_sample_list_clear(samples);

		ok(0 == hkl_sample_list_len(samples), __func__);
	}

	/* also release sample1 and sample2 */
	hkl_sample_list_free(samples);
}

int main(int argc, char** argv)
{
	plan(50);

	new();
	add_reflection();
	get_reflection();
	del_reflection();
	set_UB();
	compute_UB_busing_levy();
	affine();
	get_reflections_xxx_angle();

	reflection_set_geometry();

	list_new();
	list_append_sample();
	list_select_current();
	list_clear();

	return 0;
}

#undef SET_ANGLES
