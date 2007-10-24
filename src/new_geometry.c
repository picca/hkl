#include "new_geometry.h"
void free_geometry_config(struct hkl_geometry_config * config)
{
	free_axes_config(config->axes);
}

void init_geometry(struct hkl_geometry * geom)
{
	init_axes(geom->axes);
	init_holders(geom->holders);
}

void free_geometry(struct hkl_geometry * geom)
{
	free_axes(geom->axes);
	free_holders(geom->holders);
}

struct hkl_geometry_config * hkl_geometry_get_config(struct hkl_geometry const * geometry)
{
	struct hkl_geometry_config * config;
	config = malloc(sizeof(struct hkl_geometry_config));
	if (!config)
		die("Impossible to create a geometry config");
	
	config->source = geometry->source;
	config->axes = hkl_axes_get_config(geometry->axes);

	return config;
}

void hkl_geometry_set_config(struct hkl_geometry * geometry, struct hkl_geometry_config const * config)
{
	geometry->source = config->source;
	hkl_axes_set_config(geometry->axes, config->axes);
}
