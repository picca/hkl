#include <math.h>

#include <hkl/hkl-axis.h>

static HklAxisConfig hkl_axis_config_default = {{-M_PI, M_PI}, 0., 0.};

void hkl_axis_init(HklAxis *axis, char const *name, HklVector const *axis_v)
{
	axis->name = name;
	axis->axis_v = *axis_v;
	axis->config = hkl_axis_config_default;
}
