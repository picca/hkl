#ifndef _NEW_HOLDERS_H
#define _NEW_HOLDERS_H

#include "axes.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	/* forward declaration begin */
	struct hkl_holder;
	/* forward declaration end */

	struct hkl_holders {
		struct hkl_axes axes;
		unsigned int len;
		unsigned int alloc;
		struct hkl_holder *holders;
	};

	extern void hkl_holders_init(struct hkl_holders *holders);

	extern void hkl_holders_release(struct hkl_holders *holders);

	extern struct hkl_holder* hkl_holders_add_holder(struct hkl_holders *holders);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _NEW_HOLDERS_H */
