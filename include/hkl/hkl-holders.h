#ifndef __HKL_HOLDERS_H__
#define __HKL_HOLDERS_H__

#include <hkl/hkl-holder.h>

HKL_BEGIN_DECLS

typedef struct _HklHolders HklHolders;

struct _HklHolders {
	HklList *axes;
	HklList *holders;
};

extern HklHolders *hkl_holders_new(void);
extern HklHolders *hkl_holders_new_copy(HklHolders const *src);

extern void hkl_holders_free(HklHolders *holders);

extern HklHolder *hkl_holders_add_holder(HklHolders *holders);

HKL_END_DECLS

#endif /* __HKL_HOLDERS_H__ */
