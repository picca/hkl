#include "geometry.h"

/* public part */
void hkl_geometry_init(struct hkl_geometry * geometry)
{
	hkl_holders_init(&geometry->holders);
}

void hkl_geometry_release(struct hkl_geometry * geometry)
{
	hkl_holders_release(&geometry->holders);
}
