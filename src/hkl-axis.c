#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-axis.h>
#include <hkl/hkl-quaternion.h>

/***********/
/* HklAxis */
/***********/
static void hkl_axis_update(HklAxis *self)
{
	hkl_quaternion_from_angle_and_axe(&self->q, self->parent.value, &self->axis_v);
}

HklAxis *hkl_axis_new(char const *name, HklVector const *axis_v)
{
	HklAxis *self = NULL;

	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklAxis");

	hkl_axis_init(self, name, axis_v);

	return self;
}

void hkl_axis_free(HklAxis *self)
{
	if(self)
		free(self);
}

void hkl_axis_init(HklAxis *self, char const * name, HklVector const *axis_v)
{
	static HklQuaternion q0 = {{1, 0, 0, 0}};

	// base initializer
	hkl_parameter_init((HklParameter *)self, name, -M_PI, 0, M_PI,
			   HKL_FALSE, HKL_TRUE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	self->axis_v = *axis_v;
	self->q = q0;
}

void hkl_axis_set_value(HklAxis *self, double value)
{
	hkl_parameter_set_value(&self->parent, value);
	hkl_axis_update(self);
}

void hkl_axis_randomize(HklAxis *self)
{
	hkl_parameter_randomize(&self->parent);
	hkl_axis_update(self);
}

void hkl_axis_get_quaternion(HklAxis const *self, HklQuaternion *q)
{
	hkl_quaternion_from_angle_and_axe(q, ((HklParameter *)self)->value,
					  &self->axis_v);
}
