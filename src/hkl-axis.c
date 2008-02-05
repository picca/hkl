#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-axis.h>

static HklAxisConfig hkl_axis_config_default = {{-M_PI, M_PI}, 0., 0.};

HklAxis *hkl_axis_new(char const *name, HklVector const *axis_v)
{
	HklAxis *axis = NULL;

	axis = malloc(sizeof(*axis));
	if (!axis)
		die("Can not allocate memory for an Axis");

	axis->name = name;
	axis->axis_v = *axis_v;
	axis->config = hkl_axis_config_default;

	return axis;
}

HklAxis *hkl_axis_new_copy(HklAxis const *axis)
{
	HklAxis *copy = NULL;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Can not allocate memory for an Axis");

	copy->name = axis->name;
	copy->axis_v = axis->axis_v;
	copy->config = axis->config;

	return copy;
}

void hkl_axis_free(HklAxis *axis)
{
	free(axis);
}
