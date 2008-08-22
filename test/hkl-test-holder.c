#include <string.h>
#include <math.h>

#include <hkl/hkl-holder.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME holder

HKL_TEST_SUITE_FUNC(new_copy)
{
	HklAxis *axis;
	HklList *axes1, *axes2;
	HklHolder holder, copy;
	HklVector axis_v = {{1, 0, 0}};
	unsigned int i;

	axes1 = hkl_list_new();
	axes2 = hkl_list_new();
	hkl_holder_init(&holder, axes1);

	// add two different axis
	axis = hkl_holder_add_rotation_axis(&holder, "a", 1, 0, 0);
	axis = hkl_holder_add_rotation_axis(&holder, "b", 1, 0, 0);

	// can not copy as axes1 and axes2 are not compatible
	HKL_ASSERT_EQUAL(HKL_FAIL, hkl_holder_init_copy(&copy, axes2, &holder));

	// so set a compatible axes2 and copy the holder
	axis = hkl_axis_new("a", &axis_v);
	hkl_list_append(axes2, axis);
	axis = hkl_axis_new("b", &axis_v);
	hkl_list_append(axes2, axis);

	HKL_ASSERT_EQUAL(HKL_SUCCESS, hkl_holder_init_copy(&copy, axes2, &holder));
	// check that private_axes are the same
	for(i=0; i<holder.private_axes->len; ++i) {
		HklAxis *axis_src = holder.private_axes->list[i];
		HklAxis *axis_copy = copy.private_axes->list[i];

		HKL_ASSERT_EQUAL(0, strcmp(axis_src->name, axis_copy->name));
	}
	// release the axes memory as holder do not manage it.
	for(i=0; i<axes1->len; ++i)
		hkl_axis_free(axes1->list[i]);
	hkl_list_free(axes1);

	for(i=0; i<axes2->len; ++i)
		hkl_axis_free(axes2->list[i]);
	hkl_list_free(axes2);

	hkl_holder_release_memory(&holder);
	hkl_holder_release_memory(&copy);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(add_rotation_axis)
{
	HklAxis *axis = NULL;
	HklList *axes = NULL;
	HklHolder holder;
	unsigned int i;

	axes = hkl_list_new();
	hkl_holder_init(&holder, axes);

	// add two different axis
	axis = hkl_holder_add_rotation_axis(&holder, "a", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	HKL_ASSERT_EQUAL(1, hkl_holder_size(&holder));
	axis = hkl_holder_add_rotation_axis(&holder, "b", 1, 0, 0);
	HKL_ASSERT_EQUAL(0, !axis);
	HKL_ASSERT_EQUAL(2, hkl_holder_size(&holder));

	// can not add two times the same axes, must return the same axis
	axis = hkl_holder_add_rotation_axis(&holder, "a", 1, 0, 0);
	HKL_ASSERT_POINTER_EQUAL(NULL, axis);
	HKL_ASSERT_EQUAL(2, hkl_holder_size(&holder));

	// release the axes memory as holder do not manage it.
	for(i=0; i<axes->len; ++i)
		hkl_axis_free(axes->list[i]);
	hkl_list_free(axes);

	hkl_holder_release_memory(&holder);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_FUNC(update)
{
	HklAxis *axis = NULL;
	HklAxisConfig config;
	HklList *axes = NULL;
	HklHolder holder;
	unsigned int i;

	axes = hkl_list_new();
	hkl_holder_init(&holder, axes);

	axis = hkl_holder_add_rotation_axis(&holder, "a", 1, 0, 0);

	hkl_holder_update(&holder);
	HKL_ASSERT_DOUBLES_EQUAL(1.0, holder.q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, holder.q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, holder.q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, holder.q.data[3], HKL_EPSILON);

	hkl_axis_get_config(axis, &config);
	config.value = M_PI_2;
	hkl_axis_set_config(axis, &config);
	hkl_holder_update(&holder);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2), holder.q.data[0], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(1./sqrt(2), holder.q.data[1], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, holder.q.data[2], HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(.0, holder.q.data[3], HKL_EPSILON);

	// release the axes memory as holder do not manage it.
	for(i=0; i<axes->len; ++i)
		hkl_axis_free(axes->list[i]);
	hkl_list_free(axes);

	hkl_holder_release_memory(&holder);

	return HKL_TEST_PASS;
}

/*

HKL_TEST_SUITE_FUNC(get_distance)
{
	HklAxes *axes1 = NULL;
	HklAxes *axes2 = NULL;
	unsigned int i;

	HklAxis *A, *B;

	HklVector axis_v = {{0, 0, 1}};

	axes1 = hkl_axes_new();
	A = hkl_axes_add_rotation(axes1, "omega", &axis_v);

	axes2 = hkl_axes_new();
	B = hkl_axes_add_rotation(axes2, "omega", &axis_v);

	A->config.current = 10 * HKL_DEGTORAD;
	A->config.consign = 10 * HKL_DEGTORAD;
	B->config.current =-10 * HKL_DEGTORAD;
	B->config.consign =-10 * HKL_DEGTORAD;

	// get_distance
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 90 * HKL_DEGTORAD;
	B->config.current =-90 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 120 * HKL_DEGTORAD;
	B->config.current =-150 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current =-240 * HKL_DEGTORAD;
	B->config.current = 200 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = 200 * HKL_DEGTORAD;
	B->config.current = 240 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.current = -90 * HKL_DEGTORAD;
	B->config.current =-100 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(20 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 90 * HKL_DEGTORAD;
	B->config.consign =-90 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(180 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 120 * HKL_DEGTORAD;
	B->config.consign =-150 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(90 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign =-240 * HKL_DEGTORAD;
	B->config.consign = 200 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(80 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = 200 * HKL_DEGTORAD;
	B->config.consign = 240 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(40 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	A->config.consign = -90 * HKL_DEGTORAD;
	B->config.consign =-100 * HKL_DEGTORAD;
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance(&axes1, &axes2), HKL_EPSILON);
	HKL_ASSERT_DOUBLES_EQUAL(10 * HKL_DEGTORAD, hkl_axes_get_distance_consign(&axes1, &axes2), HKL_EPSILON);

	for(i=0; i<axes1->axes->len; ++i)
		hkl_axis_free(axes1->axes->list[i]);
	for(i=0; i<axes2->axes->len; ++i)
		hkl_axis_free(axes2->axes->list[i]);

	hkl_axes_free(axes1);
	hkl_axes_free(axes2);

	return HKL_TEST_PASS;
}
*/

HKL_TEST_SUITE_BEGIN

HKL_TEST( new_copy );
HKL_TEST( add_rotation_axis );
HKL_TEST( update );

HKL_TEST_SUITE_END
