#include <string.h>

#include <hkl/hkl-holder.h>

/* private part */
static void hkl_holder_grow(HklHolder * holder, size_t extra)
{
	if (holder->len + extra <= holder->len)
		die("you want to use way too much memory");
	if (!holder->alloc)
		holder->private_axes = NULL;
	ALLOC_GROW(holder->private_axes, holder->len + extra, holder->alloc);
}

/* public part */
void hkl_holder_init(HklHolder * holder, HklAxes *axes)
{
	holder->axes = axes;
	holder->len = 0;
	holder->alloc = 0;
	holder->private_axes = NULL;
}

void hkl_holder_release(HklHolder * holder)
{
	if (holder->alloc) {
		free(holder->private_axes);
		hkl_holder_init(holder, holder->axes);
	}
}

HklAxis* hkl_holder_add_rotation_axis(HklHolder * holder, char const * name, double x, double y, double z)
{
	size_t i;
	HklAxis *axis;
	HklVector axis_v = {{x, y, z}};

	axis = hkl_axes_add_rotation(holder->axes, name, &axis_v);
	if (axis) {
		/* check that the axis is not already in the holder */
		for(i=0; i<holder->len; i++)
			if (axis == holder->private_axes[i])
				die("can not add two times the \"%s\" axis to an holder.", name);
	} else
		die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an holder",
				name,
				axis->axis_v.data[0], axis->axis_v.data[1], axis->axis_v.data[2],
				axis_v.data[0], axis_v.data[1], axis_v.data[2]);
	hkl_holder_grow(holder, 1);
	holder->private_axes[holder->len++] = axis;
	return axis;
}
