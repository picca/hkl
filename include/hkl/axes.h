#ifndef _AXES_H
#define _AXES_H

#include <stdlib.h>

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	/* forward declaration begin */
	struct hkl_svector;
	struct hkl_axis;
	/* forward declaration end */

	struct hkl_axes {
		size_t alloc;
		size_t len;
		struct hkl_axis *axes;
	};

	extern void hkl_axes_init(struct hkl_axes *axes);

	extern void hkl_axes_release(struct hkl_axes *axes);

	extern struct hkl_axis * hkl_axes_add_rotation(struct hkl_axes *axes, char const *name, struct hkl_svector const *axis_v);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _AXES_H */
