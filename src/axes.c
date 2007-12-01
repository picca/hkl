#include <math.h>
#include <string.h>

#include "config.h"
#include "axis.h"
#include "axes.h"

static void hkl_axes_grow(struct hkl_axes *axes, size_t extra)
{
	if (axes->len + extra <= axes->len)
		die("you want to use way too much memory");
	if (!axes->alloc)
		axes->axes = NULL;
	ALLOC_GROW(axes->axes, axes->len + extra, axes->alloc);
}

void hkl_axes_init(struct hkl_axes *axes)
{
	axes->alloc = axes->len = 0;
	axes->axes = NULL;
}

void hkl_axes_release(struct hkl_axes *axes)
{
	if (axes->alloc) {
		free(axes->axes);
		hkl_axes_init(axes);
	}
}

/* 
 * Try to add a axis to the axes list,
 * if a identical axis is present in the list return it
 * else create a new on and add it to the list.
 * return NULL, if an identical axis with a different axe is present in the list
 */
struct hkl_axis* hkl_axes_add_rotation(struct hkl_axes *axes, char const *name, struct hkl_svector const *axis_v)
{
	size_t i;
	struct hkl_axis *axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0;i<axes->len;i++)
		if (strcmp(axes->axes[i].name, name) == 0) {
			if (hkl_svector_cmp(&axes->axes[i].axis_v, axis_v))
				return NULL;
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
