#include <string.h>

#include <hkl/hkl-list.h>

/* private */

static void hkl_list_grow(HklList *list, size_t extra)
{
	if (list->len + extra <= list->len)
		die("you want to use way too much memory");
	if (!list->alloc)
		list->list = NULL;
	ALLOC_GROW(list->list, list->len + extra, list->alloc);
}

static void hkl_list_init(HklList *list)
{
	list->alloc = list->len = 0;
	list->free = NULL;
	list->list = NULL;
}

/* public */

HklList* hkl_list_new(void)
{
	HklList *list = NULL;
	list = malloc(sizeof(*list));
	if (!list)
		die("Cannot allocate memory");
	
	hkl_list_init(list);

	return list;
}

HklList* hkl_list_new_managed(void (*free)(void *))
{
	HklList *list = NULL;
	
	list = hkl_list_new();
	list->free = free;

	return list;
}

void hkl_list_free(HklList *list)
{
	size_t i;

	if (list->alloc) {
		if (list->free)
			for(i=0; i<list->len; ++i)
				(*list->free)(list->list[i]);
		free(list->list);
	}
	free(list);
}

void hkl_list_append(HklList *list, void *data)
{
	hkl_list_grow(list, 1);
	list->list[list->len++] = data;
}

int hkl_list_del_by_idx(HklList *list, size_t idx)
{
	if (idx >= list->len)
		return HKL_FAIL;

	if (list->free)
		(*list->free)(list->list[idx]);
	list->len--;
	if (idx < list->len)
		memmove(&list->list[idx], &list->list[idx] + 1,
				sizeof(void *) * (list->len - idx));
	return HKL_SUCCESS;
}

size_t hkl_list_get_idx(HklList const *list, void *item)
{
	size_t i;

	for(i=0; i<list->len; ++i)
		if (list->list[i] == item)
			return i;

	return -1;
}

void hkl_list_foreach(HklList *list, void (*f)(void *))
{
	size_t i;

	for(i=0; i<list->len; ++i)
		(*f)(list->list[i]);
}
