#include <math.h>
#include <string.h>

#include <hkl/hkl-axes.h>

/* public */

HklAxes* hkl_axes_new(void)
{
	HklAxes *axes = malloc(sizeof(*axes));
	if (!axes)
		die("Can not alloc memory for a HklAxis");
	axes->axes = hkl_list_new();

	return axes;
}

void hkl_axes_free(HklAxes *axes)
{
	hkl_list_free(axes->axes);
	free(axes);
}

/* 
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
HklAxis *hkl_axes_add_rotation(HklAxes *axes,
		char const *name, HklVector const *axis_v)
{
	size_t i;
	HklAxis *axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0;i<axes->axes->len;i++) {
		axis = axes->axes->list[i];
		if (strcmp(axis->name, name) == 0) {
			if (!hkl_vector_cmp(&axis->axis_v, axis_v))
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
						name,
						axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
						axis_v->data[0], axis_v->data[1], axis_v->data[2]);
			else
				return axis;
		}
	}

	// no so create and add it to the list
	axis = hkl_axis_new(name, axis_v);
	hkl_list_append(axes->axes, axis);

	return axis;
}
