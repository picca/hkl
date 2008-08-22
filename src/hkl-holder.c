#include <string.h>

#include <hkl/hkl-geometry.h>
#include <hkl/hkl-quaternion.h>

/* private */

/* 
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
static HklAxis *hkl_axes_add_rotation(HklGeometry *geometry,
		char const *name, HklVector const *axis_v)
{
	size_t i;
	HklAxis *axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0; i<geometry->axes_len; i++) {
		axis = geometry->axes[i];
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
	geometry->axes = realloc( geometry->axes, (geometry->axes_len + 1) * sizeof(HklAxis*));
	axis = malloc(sizeof(HklAxis));
	hkl_axis_init(axis, name, axis_v);
	return geometry->axes[geometry->axes_len++] = axis;
}

static int hkl_holder_is_dirty(HklHolder const *holder)
{
	size_t i;

	for(i=0; i<holder->axes_len; ++i) {
		if (holder->axes[i]->config.dirty)
			return HKL_TRUE;
	}
	return HKL_FALSE;
}

/* public part */
void hkl_holder_init(HklHolder *self, HklGeometry *geometry)
{
	self->geometry = geometry;
	self->axes = NULL;
	self->axes_len = 0;
	hkl_quaternion_init(&self->q, 0, 0, 0, 0);
}

int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
		HklHolder const *holder)
{
	size_t i, j;

	// check axes compatibility
	if (geometry->axes_len != holder->geometry->axes_len)
		return HKL_FAIL;

	self->geometry = geometry;
	self->axes = malloc(holder->axes_len * sizeof(HklAxis*));
	self->axes_len = holder->axes_len;
	for(i=0; i<holder->axes_len; ++i)
		for(j=0; j<holder->geometry->axes_len; ++j)
			if (holder->geometry->axes[j] == holder->axes[i])
				self->axes[i] = geometry->axes[j];
	self->q = holder->q;

	return HKL_SUCCESS;
}

void hkl_holder_release_memory(HklHolder *holder)
{
	free(holder->axes), holder->axes = NULL, holder->axes_len = 0;
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder * holder,
		char const * name, double x, double y, double z)
{
	size_t i;
	HklAxis *axis;
	HklVector axis_v = {{x, y, z}};

	if ((axis = hkl_axes_add_rotation(holder->geometry, name, &axis_v))) {
		/* check that the axis is not already in the holder */
		for(i=0; i<holder->axes_len; i++)
			if (axis == holder->axes[i])
				return NULL;

		holder->axes = realloc(holder->axes,
				(holder->axes_len + 1) * sizeof(HklAxis *));
		holder->axes[holder->axes_len++] = axis;
	}
	return axis;
}

size_t hkl_holder_size(HklHolder const *holder)
{
	return holder->axes_len;
}

void hkl_holder_update(HklHolder *holder)
{
	if (hkl_holder_is_dirty(holder)) {
		size_t i;

		hkl_quaternion_init(&holder->q, 1, 0, 0, 0);
		for(i=0; i<holder->axes_len; ++i) {
			HklAxis *axis;
			HklQuaternion q;

			axis = holder->axes[i];
			hkl_axis_get_quaternion(axis, &q);
			hkl_quaternion_times_quaternion(&holder->q, &q);
		}
	}
}
