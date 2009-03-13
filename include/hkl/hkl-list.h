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
	void **data;
};

#define HKL_LIST(type, name) type *name; size_t name ## _len

#define HKL_LIST_INIT(array) array = NULL, array ## _len = 0

#define HKL_LIST_ALLOC(array, len) do{			\
		array = malloc(len * sizeof(*array));	\
		array ## _len = len;			\
	}while(0)

#define HKL_LIST_FREE(array) free(array), HKL_LIST_INIT(array)

#define HKL_LIST_ADD(array) do{					\
		array = realloc(array, ++array ## _len * sizeof(*array)); \
	}while(0)

extern HklList *hkl_list_new(void);
extern HklList *hkl_list_new_managed(void *(*copy)(void const *),
				     void (*free)(void *));
extern HklList *hkl_list_new_copy(HklList const *self);

extern void hkl_list_free(HklList *self);

extern void hkl_list_append(HklList *self, void *data);

extern void *hkl_list_get_by_idx(HklList *self, size_t idx);

/* TODO test */
extern int hkl_list_del_item(HklList *self, void *item);

extern int hkl_list_del_by_idx(HklList *self, size_t idx);

extern size_t hkl_list_get_idx(HklList const *self, void * item);

extern void hkl_list_foreach(HklList *self, void (*f)(void *));

HKL_END_DECLS

#endif
