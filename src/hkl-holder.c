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
	size_t i, len;
	HklAxis *axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0; i<geometry->axes_len; i++) {
		axis = geometry->axes[i];
		if (strcmp(((HklParameter *)(axis))->name, name) == 0) {
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
	len = geometry->axes_len++;
	geometry->axes = realloc(geometry->axes, geometry->axes_len * sizeof(HklAxis*));
	axis = hkl_axis_new(name, axis_v);
	return geometry->axes[len] = axis;
}

/* public part */
void hkl_holder_init(HklHolder *self, HklGeometry *geometry)
{
	static HklQuaternion q0 = {{0, 0, 0, 0}};
	self->geometry = geometry;
	self->axes = NULL;
	self->axes_len = 0;
	self->q = q0;
}

int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
		HklHolder const *holder)
{
	size_t i, idx;
	// check axes compatibility
	if (geometry->axes_len != holder->geometry->axes_len)
		return HKL_FAIL;

	self->geometry = geometry;
	self->axes = malloc(holder->axes_len * sizeof(HklAxis*));
	self->axes_len = holder->axes_len;
	for(i=0; i<holder->axes_len; ++i)
		for(idx=0; idx<holder->geometry->axes_len; ++idx)
			if (holder->geometry->axes[idx] == holder->axes[i])
				self->axes[i] = geometry->axes[idx];
	self->q = holder->q;

	return HKL_SUCCESS;
}

void hkl_holder_release_memory(HklHolder *self)
{
	free(self->axes), self->axes = NULL, self->axes_len = 0;
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder * self,
		char const * name, double x, double y, double z)
{
	HklAxis *axis = NULL;
	HklVector axis_v = {{x, y, z}};

	if ((axis = hkl_axes_add_rotation(self->geometry, name, &axis_v))) {
		size_t i, len;

		/* check that the axis is not already in the holder */
		for(i=0; i<self->axes_len; i++)
			if (axis == self->axes[i])
				return NULL;

		len = self->axes_len++;
		self->axes = realloc(self->axes, self->axes_len * sizeof(HklAxis *));
		self->axes[len] = axis;
	}
	return axis;
}
