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

#define HKL_LIST_LEN(array) array ## _len

#define HKL_LIST(type, name) type *name; size_t HKL_LIST_LEN(name)

#define HKL_LIST_INIT(array) array = NULL, HKL_LIST_LEN(array) = 0

#define HKL_LIST_ALLOC(array, len) do{			\
		array = malloc((len) * sizeof(*array));	\
		HKL_LIST_LEN(array) = (len);		\
	}while(0)

#define HKL_LIST_COPY(dst, src) memcpy(dst, src, HKL_LIST_LEN(src) * sizeof(*src))

#define HKL_LIST_FREE(array) free(array), HKL_LIST_INIT(array)

#define HKL_LIST_FREE_DESTRUCTOR(array, destructor) do{		\
		if(HKL_LIST_LEN(array)){			\
			size_t i;				\
			for(i=0; i<HKL_LIST_LEN(array); ++i)	\
				destructor(array[i]);		\
		}						\
		HKL_LIST_FREE(array);				\
	}while(0)

#define HKL_LIST_RESIZE(array, len) do{				\
		array = realloc(array, (len) * sizeof(*array));	\
		HKL_LIST_LEN(array) = (len);			\
	}while(0)

#define HKL_LIST_ADD_VALUE(array, value) do{				\
		size_t len = HKL_LIST_LEN(array);			\
		HKL_LIST_RESIZE(array, len + 1);			\
		array[len] = value;					\
	}while(0)

#define HKL_LIST_DEL(array, idx) do{					\
		HKL_LIST_LEN(array) = HKL_LIST_LEN(array) - 1;		\
		if (idx < HKL_LIST_LEN(array))				\
			memmove(&array[idx], &array[idx] + 1, sizeof(*array) * (HKL_LIST_LEN(array) - idx)); \
	}while(0)

#define HKL_LIST_DEL_ITEM_DESTRUCTOR(array, item, destructor) do{	\
		size_t i;						\
		for(i=0; i<HKL_LIST_LEN(array); ++i)			\
			if(array[i] == item){				\
				destructor(array[i]);			\
				HKL_LIST_DEL(array, i);			\
			}						\
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
