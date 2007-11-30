#ifndef _DIFFRACTOMETER_2C_H_
#define _DIFFRACTOMETER_2C_H_

#include "diffractometer.h"
#include "holder.h"
#include "holders.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	static void init_2C_geometry(struct hkl_geometry * geometry)
	{
		struct hkl_holder * holder = NULL;

		hkl_source_init(&geometry->source, 1.54, 1, 0, 0);

		/* sample */
		holder = hkl_holders_add_holder(&geometry->holders);
		hkl_holder_add_rotation_axis(holder, "omega", 0, -1, 0);

		/* detector */
		holder = hkl_holders_add_holder(&geometry->holders);
		hkl_holder_add_rotation_axis(holder, "tth", 0, -1, 0);
	}

	static void init_2C_diffractometer(struct hkl_diffractometer *diffractometer)
	{
		init_2C_geometry(&diffractometer->geometry);
	}

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _DIFFRACTOMETER_2C_H_ */
