#ifndef _GEOMETRY_H
#define _GEOMETRY_H

#include "source.h"
#include "axes.h"
#include "holders.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif
	/* forward declaration begin */
	struct hkl_holder;
	/* forward declaration end */

	struct hkl_geometry {
		struct hkl_source source;
		struct hkl_axes axes;
		struct hkl_holders holders;
	};

	extern void hkl_geometry_init(struct hkl_geometry *geometry);

	extern struct hkl_holder * hkl_geometry_add_holder(struct hkl_geometry *geometry);

	extern void hkl_geometry_release(struct hkl_geometry *geometry);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _GEOMETRY_H */
