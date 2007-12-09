#ifndef __HKL_DIFFRACTOMETER_2C_H__
#define __HKL_DIFFRACTOMETER_2C_H__

#include <hkl/hkldiffractometer.h>

HKL_BEGIN_DECLS

	static void init_2C_geometry(HklGeometry * geometry)
	{
		HklHolder * holder = NULL;

		hkl_source_init(&geometry->source, 1.54, 1, 0, 0);

		/* sample */
		holder = hkl_holders_add_holder(&geometry->holders);
		hkl_holder_add_rotation_axis(holder, "omega", 0, -1, 0);

		/* detector */
		holder = hkl_holders_add_holder(&geometry->holders);
		hkl_holder_add_rotation_axis(holder, "tth", 0, -1, 0);
	}

	static void init_2C_diffractometer(HklDiffractometer *diffractometer)
	{
		init_2C_geometry(&diffractometer->geometry);
	}

HKL_END_DECLS

#endif /* __HKL_DIFFRACTOMETER_2C_H__ */
