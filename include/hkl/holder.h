#ifndef _NEW_HOLDER_H
#define _NEW_HOLDER_H

#include "axis.h"
#include "quaternion.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

/* forward declaration */
struct hkl_svector;

struct hkl_holder {
	struct hkl_axes * axes;
	unsigned int len;
	unsigned int alloc;
	unsigned int *idx;

	int dirty;
	struct hkl_quaternion _q;
	struct hkl_quaternion _q_consign;
};

struct hkl_holders {
	struct hkl_axes * axes;
	unsigned int len;
	unsigned int alloc;
	struct hkl_holder *holders;
};

extern struct hkl_axis * hkl_holder_add_rotation_axis(struct hkl_holder * holder, char const * name, struct hkl_svector const * rot_axis);

extern void hkl_holder_apply_quaternion(struct hkl_holder const * holder, struct hkl_quaternion * q); /* TODO */

extern void hkl_holder_apply_quaternion_consign(struct hkl_holder const * holder, struct hkl_quaternion * q); /* TODO */

extern struct hkl_holders * hkl_holders_new(void); /* TODO */

extern struct hkl_holder * hkl_holders_add_holder(struct hkl_holders * holders); /* TODO */

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _NEW_HOLDER_H */
