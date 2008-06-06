#include <string.h>

#include <hkl/hkl-holder.h>
#include <hkl/hkl-quaternion.h>

/* private */

/* 
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
static HklAxis *hkl_axes_add_rotation(HklList *axes,
		char const *name, HklVector const *axis_v)
{
	size_t i;
	HklAxis *axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0; i<axes->len; i++) {
		axis = axes->list[i];
		if (strcmp(axis->name, name) == 0) {
			if (hkl_vector_cmp(&axis->axis_v, axis_v)) {
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
						name,
						axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
						axis_v->data[0], axis_v->data[1], axis_v->data[2]);
				return NULL;
			} else
				return axis;
		}
	}

	// no so create and add it to the list
	axis = hkl_axis_new(name, axis_v);
	hkl_list_append(axes, axis);

	return axis;
}

static int hkl_holder_is_dirty(HklHolder const *holder)
{
	size_t i;

	for(i=0; i<holder->private_axes->len; ++i) {
		HklAxis *axis = holder->private_axes->list[i];
		if (axis->config.dirty)
			return HKL_TRUE;
	}
	return HKL_FALSE;
}

/* public part */
HklHolder *hkl_holder_new(HklList *axes)
{
	HklHolder *holder = malloc(sizeof(*holder));
	if(!holder)
		die("Cannot allocate the memory for an HklHolder");

	holder->axes = axes;
	holder->private_axes = hkl_list_new();
	holder->q = hkl_quaternion_new(0, 0, 0, 0);

	return holder;
}

HklHolder *hkl_holder_new_copy(HklHolder *src, HklList *axes)
{
	HklHolder *copy;
	unsigned int i;

	// check axes compatibility
	if (axes->len != src->axes->len){
		//warning("Non compatible axes -> cannot copy hklHolder");
		return NULL;
	}

	copy = malloc(sizeof(*copy));
	if(!copy)
		die("Cannot allocate the memory for an HklHolder");

	copy->axes = axes;
	copy->private_axes = hkl_list_new();
	for(i=0; i<src->private_axes->len; ++i) {
		size_t idx;

		idx = hkl_list_get_idx(src->axes, src->private_axes->list[i]);
		hkl_list_append(copy->private_axes, axes->list[idx]);
	}
	copy->q = hkl_quaternion_new_copy(src->q);

	return copy;
}

void hkl_holder_free(HklHolder *holder)
{
	hkl_list_free(holder->private_axes);
	hkl_quaternion_free(holder->q);
	free(holder);
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder * holder,
		char const * name, double x, double y, double z)
{
	size_t i;
	HklAxis *axis;
	HklVector axis_v = {{x, y, z}};

	if ((axis = hkl_axes_add_rotation(holder->axes, name, &axis_v))) {
		/* check that the axis is not already in the holder */
		for(i=0; i<holder->private_axes->len; i++)
			if (axis == holder->private_axes->list[i])
				return NULL;

		hkl_list_append(holder->private_axes, axis);
	}
	return axis;
}

size_t hkl_holder_size(HklHolder const *holder)
{
	return holder->private_axes->len;
}

void hkl_holder_update(HklHolder *holder)
{
	if (hkl_holder_is_dirty(holder)) {
		size_t i;

		hkl_quaternion_set(holder->q, 1, 0, 0, 0);
		for(i=0; i<holder->private_axes->len; ++i) {
			HklAxis *axis;
			HklQuaternion q;

			axis = holder->private_axes->list[i];
			hkl_axis_get_quaternion(axis, &q);
			hkl_quaternion_times_quaternion(holder->q, &q);
		}
	}
}
