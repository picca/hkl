#include "config.h"
#include "axis.h"

static struct hkl_axis_config hkl_axis_config_default = {{-M_PI, M_PI}, 0., 0.};

void hkl_axis_init(struct hkl_axis *axis, name, struct hkl_svector *axis_v)
{
	axis->name = name;
	axis->axis_v = *axis_v;
	axis->config = hkl_axis_config_default;
}
