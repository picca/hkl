#include <math.h>
#include <string.h>

#include "config.h"
#include "axis.h"

/* private axis part */

static struct hkl_axis_config hkl_axis_config_default = {{-M_PI, M_PI}, 0., 0.};

/* private axes part */

void hkl_axes_grow(struct hkl_axes * axes, size_t extra)
{
	if (axes->len + extra <= axes->len)
		die("you want to use way too much memory");
	if (!axes->alloc)
		axes->axes = NULL;
	ALLOC_GROW(axes->axes, axes->len + extra, axes->alloc);
}

void hkl_axes_init(struct hkl_axes * axes, size_t hint)
{
	axes->alloc = axes->len = 0;
	if (hint)
		hkl_axes_grow(axes, hint);
	else
		axes->axes = NULL;
}

void hkl_axes_release(struct hkl_axes * axes)
{
	if (axes->alloc) {
		free(axes->axes);
		hkl_axes_init(axes, 0);
	}
}

/* private axes_config part */
struct hkl_axes_config * create_axes_config(size_t hint)
{
	struct hkl_axes_config * config;
	
	config = malloc(sizeof(struct hkl_axes_config));
	if (config)
		die("Can not create the axes config");

	config->len = hint;
	config->configs = malloc(hint * sizeof(struct hkl_axis_config));
	if(!config->configs)
		die("Can not create the axes config");

	return config;
}

/* public axes part*/

/** @brief add a rotation axis to the axe list */
struct hkl_axis * hkl_axes_add_rotation(struct hkl_axes * axes, char const * name, struct hkl_svector const * rot_axis)
{
	size_t i;
	struct hkl_axis * axis = NULL;

	// check if an axis with the same name is in the axis list.
	for (i=0;i<axes->len;i++)
		if (strcmp(axes->axes[i].name, name) == 0)
			die("can not add two axis with the same name \"%s\" in the axe list.", name);

	// no so we can add the axe
	// first grow the axes list.
	hkl_axes_grow(axes, 1);

	// set the right parameters of the axe
	axis = &axes->axes[axes->len];
	axes->len++;
	axis->name = name;
	axis->axis = *rot_axis;
	axis->config = hkl_axis_config_default;

	return axis;
}

/** return the index of an axis in the axes list */
size_t hkl_axes_get_idx_by_name(struct hkl_axes * axes, char const * name)
{
	size_t i;
	
	for (i=0;i<axes->len;i++)
		if (strcmp(axes->axes[i].name, name) == 0)
			return i;

	return -1;
}

/** make a deep copy of an hkl_axes */
struct hkl_axes * hkl_axes_copy(struct hkl_axes const * src)
{
	struct hkl_axes * dst = NULL;
	size_t i;

	hkl_axes_init(dst, src->len);
	for(i=0;i<src->len;i++)
		dst->axes[i] = src->axes[i];
	
	return dst;
}

double get_rotation_distance(double d1, double d2)
{
	return acos(cos(fmod(d1, 2. * M_PI) - fmod(d2, 2. * M_PI)));
}

/** get the distance of the axes */
double hkl_axes_get_distance(struct hkl_axes const * axes1, struct hkl_axes const * axes2)
{
	size_t i;
	double distance = 0;

	if (axes1->len == axes2->len) {
		for(i=0;i<axes1->len;i++)
			distance += get_rotation_distance(axes1->axes[i].config.current,
							  axes2->axes[i].config.current);
	}

	return distance;
}

double hkl_axes_get_distance_consign(struct hkl_axes const * axes1, struct hkl_axes const * axes2)
{
	size_t i;
	double distance = 0;

	if (axes1->len == axes2->len) {
		for(i=0;i<axes1->len;i++)
			distance += get_rotation_distance(axes1->axes[i].config.consign,
							  axes2->axes[i].config.consign);
	}

	return distance;
}

/** return the configuration of the axes the memory must be release */
struct hkl_axes_config * hkl_axes_get_config(struct hkl_axes const * axes)
{
	size_t i;
	struct hkl_axes_config * config = NULL;

	config = create_axes_config(axes->len);
	for(i=0;i<axes->len;i++)
		config->configs[i] = axes->axes[i].config;

	return config;
}

/** set the configurations of the axes */
void hkl_axes_set_config(struct hkl_axes * axes, struct hkl_axes_config const * config)
{
	size_t i;

	if (axes->len != config->len)
		die("Can not set this axe configuration, not the same number of axes");
	for(i=0;i<axes->len;i++)
		axes->axes[i].config = config->configs[i];
}

void free_axes_config(struct hkl_axes_config * config)
{
	if (config->len) {
		free(config->configs);
		config->len = 0;
	}
}
