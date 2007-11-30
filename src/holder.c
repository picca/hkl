#include <string.h>

#include "config.h"
#include "axis.h"
#include "axes.h"
#include "holder.h"
#include "svector.h"

/* private part */
static void hkl_holder_grow(struct hkl_holder * holder, size_t extra)
{
	if (holder->len + extra <= holder->len)
		die("you want to use way too much memory");
	if (!holder->alloc)
		holder->private_axes = NULL;
	ALLOC_GROW(holder->private_axes, holder->len + extra, holder->alloc);
}

/* public part */
void hkl_holder_init(struct hkl_holder * holder, struct hkl_axes *axes)
{
	holder->axes = axes;
	holder->len = 0;
	holder->alloc = 0;
	holder->private_axes = NULL;
}

void hkl_holder_release(struct hkl_holder * holder)
{
	if (holder->alloc) {
		free(holder->private_axes);
		hkl_holder_init(holder, holder->axes);
	}
}

struct hkl_axis* hkl_holder_add_rotation_axis(struct hkl_holder * holder, char const * name, double x, double y, double z)
{
	size_t i;
	struct hkl_axis *axis;
	struct hkl_svector axis_v = {{x, y, z}};

	axis = hkl_axes_add_rotation(holder->axes, name, &axis_v);
	if (axis) {
		/* check that the axis is not already in the holder */
		for(i=0; i<holder->len; i++)
			if (axis == holder->private_axes[i])
				die("can not add two times the \"%s\" axis to an holder.", name);
	} else
		die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an holder",
				name,
				axis->axis_v.data[X], axis->axis_v.data[Y], axis->axis_v.data[Z],
				axis_v.data[X], axis_v.data[Y], axis_v.data[Z]);
	hkl_holder_grow(holder, 1);
	holder->private_axes[holder->len++] = axis;
	return axis;
}
