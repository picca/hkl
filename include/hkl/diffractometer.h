#ifndef _NEW_DIFFRACTOMETER_H
#define _NEW_DIFFRACTOMETER_H

#include <stdarg.h>

#include "config.h"
#include "geometry.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	struct hkl_diffractometer {
		struct hkl_geometry geometry;
	};

	enum hkl_diffractometer_type
	{
		DIFF_TYPE_2C,
		DIFF_TYPE_E4CV,
		DIFF_TYPE_E6C,
		DIFF_TYPE_K4CV,
		DIFF_TYPE_K6C,
	};

	extern void hkl_diffractometer_init(struct hkl_diffractometer *diffractometer, enum hkl_diffractometer_type type, ...);

	extern void hkl_diffractometer_release(struct hkl_diffractometer *diffractometer);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _NEW_DIFFRACTOMETER_H */
