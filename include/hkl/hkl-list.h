#ifndef __HKL_LIST_H__
#define __HKL_LIST_H__

#include <stdlib.h>
#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklList HklList;

struct _HklList
{
	size_t len;
	size_t alloc;
	void **list;
};

extern HklList *hkl_list_new(void);

extern void hkl_list_free(HklList *list);

extern void hkl_list_append(HklList *list, void *data);

extern int hkl_list_del_by_idx(HklList *list, size_t idx);

HKL_END_DECLS

#endif
