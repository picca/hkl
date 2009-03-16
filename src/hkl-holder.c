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
static size_t hkl_axes_add_rotation(HklGeometry *geometry,
				      char const *name, HklVector const *axis_v)
{
	size_t i, len;
	HklAxis axis;

	// check if an axis with the same name is on the axis list
	for(i=0; i<HKL_LIST_LEN(geometry->axes); ++i){
		HklAxis *axis;

		axis = &geometry->axes[i];
		if(!strcmp(axis->parent.name, name)){
			if (hkl_vector_cmp(&axis->axis_v, axis_v))
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
				    name,
				    axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
				    axis_v->data[0], axis_v->data[1], axis_v->data[2]);
			else
				return i;
		}
	}

	// no so create and add it to the list
	len = HKL_LIST_LEN(geometry->axes);
	hkl_axis_init(&axis, name, axis_v);
	HKL_LIST_ADD_VALUE(geometry->axes, axis);

	return len;
}

/* public part */
void hkl_holder_init(HklHolder *self, HklGeometry *geometry)
{
	static HklQuaternion q0 = {{0, 0, 0, 0}};
	self->geometry = geometry;
	HKL_LIST_INIT(self->idx);
	self->q = q0;
}

int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
		HklHolder const *holder)
{
	// check axes compatibility
	if (HKL_LIST_LEN(geometry->axes) != HKL_LIST_LEN(holder->geometry->axes))
		return HKL_FAIL;

	self->geometry = geometry;

	HKL_LIST_ALLOC(self->idx, HKL_LIST_LEN(holder->idx));
	HKL_LIST_COPY(self->idx, holder->idx);

	self->q = holder->q;

	return HKL_SUCCESS;
}

void hkl_holder_release_memory(HklHolder *self)
{
	HKL_LIST_FREE(self->idx);
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder * self,
				      char const * name, double x, double y, double z)
{
	HklAxis *axis = NULL;
	size_t i, idx;
	HklVector axis_v = {{x, y, z}};

	idx = hkl_axes_add_rotation(self->geometry, name, &axis_v);

	/* check that the axis is not already in the holder */
	for(i=0; i<HKL_LIST_LEN(self->idx); i++)
		if (idx == self->idx[i])
			return NULL;

	axis = &self->geometry->axes[idx];
	HKL_LIST_ADD_VALUE(self->idx, idx);

	return axis;
}
