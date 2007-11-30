#ifndef _AXIS_H
#define _AXIS_H

#include "svector.h"
#include "interval.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	struct hkl_axis_config {
		struct hkl_interval range;
		double current;
		double consign;
	};

	struct hkl_axis {
		const char * name;
		struct hkl_svector axis_v;
		struct hkl_axis_config config;
	};

	extern void hkl_axis_init(struct hkl_axis *axis, char const *name, struct hkl_svector const *axis_v);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _NEW_AXE_H */
