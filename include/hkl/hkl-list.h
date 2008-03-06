#ifndef __HKL_LIST_H__
#define __HKL_LIST_H__

#include <stdlib.h>
#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

/* 
 * The HklList struct contain a list of pointer on objects
 * the memory can be managed by the list or not, but be carefull
 * it can only managed memory that can be release with the free
 */
typedef struct _HklList HklList;

struct _HklList
{
	size_t len;
	size_t alloc;
	void *(*copy)(void const *);
	void (*free)(void *);
	void **list;
};

extern HklList *hkl_list_new(void);
extern HklList *hkl_list_new_managed(void *(*copy)(void const *),
		void (*free)(void *));
extern HklList *hkl_list_new_copy(HklList const *src);

extern void hkl_list_free(HklList *list);

extern void hkl_list_append(HklList *list, void *data);

extern int hkl_list_del_by_idx(HklList *list, size_t idx);

extern size_t hkl_list_get_idx(HklList const *list, void * item);

extern void hkl_list_foreach(HklList *list, void (*f)(void *));

HKL_END_DECLS

#endif
