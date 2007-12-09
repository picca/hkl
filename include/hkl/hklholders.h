#ifndef __HKL_HOLDERS_H__
#define __HKL_HOLDERS_H__

#include <hkl/hklholder.h>

HKL_BEGIN_DECLS

typedef struct _HklHolders HklHolders;

struct _HklHolders {
	HklAxes axes;
	unsigned int len;
	unsigned int alloc;
	HklHolder *holders;
};

extern void hkl_holders_init(HklHolders *holders);

extern void hkl_holders_release(HklHolders *holders);

extern HklHolder* hkl_holders_add_holder(HklHolders *holders);

HKL_END_DECLS

#endif /* __HKL_HOLDERS_H__ */
