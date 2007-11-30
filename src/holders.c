#include "config.h"
#include "axes.h"
#include "holder.h"
#include "holders.h"

/* private hkl_holders part */
void hkl_holders_grow(struct hkl_holders * holders, size_t extra)
{
	if (holders->len + extra <= holders->len)
		die("you want to use way too much memory");
	if (!holders->alloc)
		holders->holders = NULL;
	ALLOC_GROW(holders->holders, holders->len + extra, holders->alloc);
}

/* public part */
void hkl_holders_init(struct hkl_holders * holders)
{
	hkl_axes_init(&holders->axes);
	holders->len = 0;
	holders->alloc = 0;
	holders->holders = NULL;
}

void hkl_holders_release(struct hkl_holders * holders)
{
	size_t i;

	if (holders->alloc) {
		for(i=0;i<holders->len;i++)
			hkl_holder_release(&holders->holders[i]);
		free(holders->holders);
		hkl_holders_init(holders);
	}
}

struct hkl_holder * hkl_holders_add_holder(struct hkl_holders * holders)
{
	hkl_holders_grow(holders, 1);
	return &holders->holders[holders->len++];
}
