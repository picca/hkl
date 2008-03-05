#ifndef __HKL_HOLDER_H__
#define __HKL_HOLDER_H__

#include <hkl/hkl-list.h>
#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklHolder HklHolder;

struct _HklHolder {
	HklList *axes;
	HklList *private_axes;
	HklQuaternion *q;
	HklQuaternion *qc;
};

extern HklHolder *hkl_holder_new(HklList *axes);
extern HklHolder *hkl_holder_new_copy(HklHolder *src, HklList *axes);

extern void hkl_holder_free(HklHolder *holder);

extern HklAxis *hkl_holder_add_rotation_axis(HklHolder *holder,
		char const *name, double x, double y, double z);

extern size_t hkl_holder_size(HklHolder const *holder);

extern void hkl_holder_update(HklHolder *holder);

extern void hkl_holder_apply_to_vector(HklHolder const *holder,
		HklVector *v, HklVector *vc);

HKL_END_DECLS

#endif /* _NEW_HOLDER_H */
