#include <string.h>
#include <math.h>

#include <hkl/hkl-axis.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME axis

HKL_TEST_SUITE_FUNC( new_copy )
{
	HklAxis *axis;
	HklAxis *copy;
	HklVector v = {{1, 0, 0}};

	axis = hkl_axis_new("omega", &v);

	HKL_ASSERT_EQUAL(0, strcmp("omega", axis->name));
	HKL_ASSERT_DOUBLES_EQUAL(-M_PI, axis->config.range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(M_PI, axis->config.range.max, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., axis->config.value, HKL_EPSILON);
	HKL_ASSERT_EQUAL(1, axis->config.dirty);

	copy = hkl_axis_new_copy(axis);

	HKL_ASSERT_EQUAL(0, strcmp("omega", copy->name));
	HKL_ASSERT_DOUBLES_EQUAL(-M_PI, copy->config.range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(M_PI, copy->config.range.max, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., copy->config.value, HKL_EPSILON);
	HKL_ASSERT_EQUAL(1, copy->config.dirty);

	hkl_axis_free(copy);
	hkl_axis_free(axis);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( clear_dirty )
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};

	axis = hkl_axis_new("omega", &v);

	hkl_axis_clear_dirty(axis);

	HKL_ASSERT_EQUAL(0, axis->config.dirty);

	hkl_axis_free(axis);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( get_set_config )
{
	HklAxis *axis;
	HklAxisConfig config;
	HklVector v = {{1, 0, 0}};

	axis = hkl_axis_new("omega", &v);

	hkl_axis_get_config(axis, &config);

	HKL_ASSERT_DOUBLES_EQUAL(-M_PI, config.range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(M_PI, config.range.max, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., config.value, HKL_EPSILON);
	HKL_ASSERT_EQUAL(1, config.dirty);

	config.range.min = -1.;
	config.range.max = 1.;
	config.value = 0.5;
	hkl_axis_clear_dirty(axis);
	hkl_axis_set_config(axis, &config);

	HKL_ASSERT_DOUBLES_EQUAL(-1., axis->config.range.min, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1., axis->config.range.max, HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0.5, axis->config.value, HKL_EPSILON);
	HKL_ASSERT_EQUAL(1, axis->config.dirty);

	hkl_axis_free(axis);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC( get_quaternions )
{
	HklAxis *axis;
	HklVector v = {{1, 0, 0}};
	HklAxisConfig config;
	HklQuaternion q;

	axis = hkl_axis_new("omega", &v);

	hkl_axis_get_quaternion(axis, &q);
	HKL_ASSERT_DOUBLES_EQUAL(1., q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[3], HKL_EPSILON);

	hkl_axis_get_config(axis, &config);
	config.value = -M_PI_2;
	hkl_axis_set_config(axis, &config);
	hkl_axis_get_quaternion(axis, &q);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2.), q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(-1./sqrt(2.), q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(0., q.data[3], HKL_EPSILON);

	hkl_axis_free(axis);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new_copy );
HKL_TEST( clear_dirty );
HKL_TEST( get_set_config );
HKL_TEST( get_quaternions );

HKL_TEST_SUITE_END
