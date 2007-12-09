#include <hkl/hklgeometry.h>

/* public part */
void hkl_geometry_init(HklGeometry * geometry)
{
	hkl_holders_init(&geometry->holders);
}

void hkl_geometry_release(HklGeometry * geometry)
{
	hkl_holders_release(&geometry->holders);
}
