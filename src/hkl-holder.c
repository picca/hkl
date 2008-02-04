#include <string.h>

#include <hkl/hkl-holder.h>

/* public part */
HklHolder* hkl_holder_new(HklAxes *axes)
{
	HklHolder *holder = malloc(sizeof(*holder));
	if(!holder)
		die("Cannot allocate the memory for an HklHolder");

	holder->axes = axes;
	holder->private_axes = hkl_axes_new();

	return holder;
}

void hkl_holder_free(HklHolder *holder)
{
	hkl_axes_free(holder->private_axes);
	free(holder);
}

HklAxis *hkl_holder_add_rotation_axis(HklHolder * holder,
		char const * name, double x, double y, double z)
{
	size_t i;
	HklAxis *axis;
	HklVector axis_v = {{x, y, z}};

	axis = hkl_axes_add_rotation(holder->axes, name, &axis_v);

	/* check that the axis is not already in the holder */
	for(i=0; i<holder->private_axes->axes->len; i++)
		if (axis == holder->private_axes->axes->list[i])
			die("can not add two times the \"%s\" axis to an holder.", name);

	hkl_list_append(holder->private_axes->axes, axis);
	return axis;
}
