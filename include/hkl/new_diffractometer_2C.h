#ifndef _NEW_DIFFRACTOMETER_2C_H
#define _NEW_DIFFRACTOMETER_2C_H

#include <string.h>

#include "new_diffractometer.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

static void init_2C_geometry(struct hkl_geometry * geom)
{
	struct hkl_holder * holder = NULL;
	static struct hkl_source = {1.54, {{1, 0, 0}}};

	geom->source = source;

	/* sample */
	holder = hkl_holders_add(geom->holders);
	hkl_holder_add_rotation(holder, "omega", 0, -1, 0);

	/* detector */
	holder = hkl_holders_add(geom->holders);
	hkl_holder_add_rotation(holder, "tth", 0, -1, 0);
}

static void init_2C_diffractometer(struct hkl_diffractometer * diff)
{
	
	init_2C_geometry(diff->geometry);
}

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _NEW_GEOMETRY_H */
