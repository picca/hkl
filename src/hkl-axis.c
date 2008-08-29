#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-axis.h>
#include <hkl/hkl-quaternion.h>

static HklAxisConfig hkl_axis_config_default = {{-M_PI, M_PI}, 0., 1};

void hkl_axis_init(HklAxis *self, char const *name, HklVector const *axis_v)
{
	self->name = name;
	self->axis_v = *axis_v;
	self->config = hkl_axis_config_default;
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

void hkl_axis_clear_dirty(HklAxis *axis)
{
	axis->config.dirty = HKL_FALSE;
}

void hkl_axis_get_quaternion(HklAxis const *axis, HklQuaternion *q)
{
	hkl_quaternion_from_angle_and_axe(q, axis->config.value,
			&axis->axis_v);
}

