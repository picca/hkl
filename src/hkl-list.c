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

void hkl_list_free(HklList *list)
{
	size_t i;

	if (list->alloc) {
		for(i=0; i<list->len; ++i)
			free(list->list[i]);
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

	free(list->list[idx]);
	list->len--;
	if (idx < list->len)
		memmove(&list->list[idx], &list->list[idx] + 1, sizeof(void *) * (list->len - idx));
	return HKL_SUCCESS;
}
