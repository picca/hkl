#include <math.h>
#include <string.h>

#include <hkl/hkl-axes.h>

/* private */
static void hkl_axes_grow(HklAxes *axes, size_t extra)
{
	if (axes->len + extra <= axes->len)
		die("you want to use way too much memory");
	if (!axes->alloc)
		axes->axes = NULL;
	ALLOC_GROW(axes->axes, axes->len + extra, axes->alloc);
}

/* public */

HklAxes* hkl_axes_new(void)
{
	HklAxes *axes = malloc(sizeof(*axes));
	if (!axes)
		die("Can not alloc memory for a HklAxis");
	hkl_axes_init(axes);
	return axes;
}

void hkl_axes_init(HklAxes *axes)
{
	axes->alloc = axes->len = 0;
	axes->axes = NULL;
}

void hkl_axes_release(HklAxes *axes)
{
	if (axes->alloc) {
		free(axes->axes);
		hkl_axes_init(axes);
	}
}

void hkl_axes_free(HklAxes *axes)
{
	hkl_axes_release(axes);
	free(axes);
}

/* 
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * die if try to add an axis with the same name but a different axis_v
 */
HklAxis* hkl_axes_add_rotation(HklAxes *axes, char const *name, HklVector const *axis_v)
{
	size_t i;
	HklAxis *axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0;i<axes->len;i++)
		if (strcmp(axes->axes[i].name, name) == 0) {
			if (!hkl_svector_cmp(&axes->axes[i].axis_v, axis_v))
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an HklAxes.",
						name,
						axes->axes[i].axis_v.data[0], axes->axes[i].axis_v.data[1], axes->axes[i].axis_v.data[2],
						axis_v->data[0], axis_v->data[1], axis_v->data[2]);
			else
				return &axes->axes[i];
		}

	// no so create and add it to the list
	hkl_axes_grow(axes, 1);

	// set the right parameters of the axe
	axis = &axes->axes[axes->len++];
	hkl_axis_init(axis, name, axis_v);

	return axis;
}
