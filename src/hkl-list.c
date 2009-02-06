#include <string.h>

#include <hkl/hkl-list.h>

/* private */

static void hkl_list_grow(HklList *self, size_t extra)
{
	if (self->len + extra <= self->len)
		die("you want to use way too much memory");
	if (!self->alloc)
		self->data = NULL;
	ALLOC_GROW(self->data, self->len + extra, self->alloc);
}

static void hkl_list_init(HklList *self)
{
	self->alloc = self->len = 0;
	self->free = NULL;
	self->copy = NULL;
	self->data = NULL;
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

HklList* hkl_list_new_managed(void *(*copy)(void const *), void (*free)(void *))
{
	HklList *list = NULL;
	
	list = hkl_list_new();
	list->copy = copy;
	list->free = free;

	return list;
}

HklList *hkl_list_new_copy(HklList const *self)
{
	HklList *list;

	if (self->copy && self->free) {
		size_t i;

		list = hkl_list_new_managed(self->copy, self->free);
		for(i=0; i<self->len; ++i)
			hkl_list_append(list, list->copy(self->data[i]));
	} else {
		list = hkl_list_new();
		hkl_list_grow(list, self->len);
		list->len = self->len;
		memcpy(list->data, self->data, sizeof(void *) * self->len);
	}

	return list;
}

void hkl_list_free(HklList *self)
{
	size_t i;

	if (self->alloc) {
		if (self->free)
			for(i=0; i<self->len; ++i)
				(*self->free)(self->data[i]);
		free(self->data);
	}
	free(self);
}

void hkl_list_append(HklList *self, void *data)
{
	hkl_list_grow(self, 1);
	self->data[self->len++] = data;
}

void *hkl_list_get_by_idx(HklList *self, size_t idx)
{
	if (idx >= self->len)
		return NULL;
	return self->data[idx];
}

int hkl_list_del_item(HklList *self, void *item)
{
	int res = HKL_FAIL;
	size_t i;

	if (self && item)
		for(i=0; i<self->len; ++i)
			if(self->data[i] == item)
				res = hkl_list_del_by_idx(self, i);
	return res;
}

int hkl_list_del_by_idx(HklList *self, size_t idx)
{
	if (!self || idx >= self->len)
		return HKL_FAIL;

	if (self->free)
		(*self->free)(self->data[idx]);
	self->len--;
	if (idx < self->len)
		memmove(&self->data[idx], &self->data[idx] + 1,
				sizeof(void *) * (self->len - idx));
	return HKL_SUCCESS;
}

size_t hkl_list_get_idx(HklList const *self, void *item)
{
	size_t i;

	for(i=0; i<self->len; ++i)
		if (self->data[i] == item)
			return i;

	return -1;
}

void hkl_list_foreach(HklList *self, void (*f)(void *))
{
	size_t i;

	for(i=0; i<self->len; ++i)
		(*f)(self->data[i]);
}
