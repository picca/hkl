#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-axis.h>
#include <hkl/hkl-quaternion.h>

/*****************/
/* HklAxisConfig */
/*****************/

static HklAxisConfig hkl_axis_config_default = {{-M_PI, M_PI}, 0., 1};

void hkl_axis_config_randomize(HklAxisConfig *self)
{
	double alea = (double)rand() / (RAND_MAX + 1.);
	self->value = self->range.min+(self->range.max-self->range.min)*alea;
	self->dirty = 1;
}

void hkl_axis_config_init(HklAxisConfig *self, double min, double max,
		double value, int dirty)
{
	self->range.min = min;
	self->range.max = max;
	self->value = value;
	self->dirty = dirty;
}

void hkl_axis_config_fprintf(FILE *f, HklAxisConfig *self)
{
	fprintf(f, "% f [%f : %f] (%d)", self->value,
		self->range.min, self->range.max, self->dirty);
}

/***********/
/* HklAxis */
/***********/

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
