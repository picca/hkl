#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-axis.h>
#include <hkl/hkl-quaternion.h>

static HklAxisConfig hkl_axis_config_default = {{-M_PI, M_PI}, 0., 0., 1};

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

void hkl_axis_get_config(HklAxis *axis, HklAxisConfig *config)
{
	*config = axis->config;
}

void hkl_axis_set_config(HklAxis *axis, HklAxisConfig *config)
{
	axis->config = *config;
	axis->config.dirty = HKL_TRUE;
}

void hkl_axis_get_quaternions(HklAxis const *axis,
		HklQuaternion *q, HklQuaternion *qc)
{
	hkl_quaternion_from_angle_and_axe(q, axis->config.current,
			&axis->axis_v);
	hkl_quaternion_from_angle_and_axe(qc, axis->config.consign,
			&axis->axis_v);
}

void hkl_axis_clear_dirty(HklAxis *axis)
{
	axis->config.dirty = HKL_FALSE;
}
