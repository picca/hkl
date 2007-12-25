#include <hkl/hkl-geometry.h>

/* public part */

HklGeometry* hkl_geometry_new(void)
{
	HklGeometry *geometry = NULL;
	geometry = malloc(sizeof(*geometry));
	if (!geometry)
		die("Cannot allocate a HklGeometry struct !!!");
	geometry->source  = hkl_source_new(1.54, 1, 0, 0);
	geometry->holders = hkl_holders_new();

	return geometry;
}

void hkl_geometry_free(HklGeometry *geometry)
{
	hkl_source_free(geometry->source);
	hkl_holders_free(geometry->holders);
	free(geometry);
}
