#include <hkl/hkl-holders.h>

/* private */
void hkl_holders_grow(HklHolders *holders, size_t extra)
{
	if (holders->len + extra <= holders->len)
		die("you want to use way too much memory");
	if (!holders->alloc)
		holders->holders = NULL;
	ALLOC_GROW(holders->holders, holders->len + extra, holders->alloc);
}

/* public */
HklHolders* hkl_holders_new(void)
{
	HklHolders *holders = malloc(sizeof(*holders));
	if (!holders)
		die("Cannot allocate memory for an HklHolders");
	
	holders->axes = hkl_axes_new();
	holders->len = 0;
	holders->alloc = 0;
	holders->holders = NULL;

	return holders;
}

void hkl_holders_free(HklHolders *holders)
{
	size_t i;

	if (holders->alloc) {
		for(i=0;i<holders->axes->axes->len;i++)
			hkl_axis_free(holders->axes->axes->list[i]);
		hkl_axes_free(holders->axes);

		for(i=0;i<holders->len;i++)
			hkl_holder_release(&holders->holders[i]);
		free(holders->holders);
	}

	free(holders);
}

HklHolder* hkl_holders_add_holder(HklHolders *holders)
{
	HklHolder *holder = NULL;
	hkl_holders_grow(holders, 1);
	holder = &holders->holders[holders->len++];
	hkl_holder_init(holder, holders->axes);

	return holder;
}
