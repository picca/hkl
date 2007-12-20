#include <hkl/hkl-holders.h>

/* private hkl_holders part */
void hkl_holders_grow(HklHolders *holders, size_t extra)
{
	if (holders->len + extra <= holders->len)
		die("you want to use way too much memory");
	if (!holders->alloc)
		holders->holders = NULL;
	ALLOC_GROW(holders->holders, holders->len + extra, holders->alloc);
}

/* public part */
HklHolders* hkl_holders_new(void)
{
	HklHolders *holders = malloc(sizeof(holders));
	if (!holders)
		die("Cannot allocate memory for an HklHolders");
	hkl_holders_init(holders);
	return holders;
}

void hkl_holders_init(HklHolders *holders)
{
	hkl_axes_init(&holders->axes);
	holders->len = 0;
	holders->alloc = 0;
	holders->holders = NULL;
}

void hkl_holders_release(HklHolders *holders)
{
	size_t i;

	if (holders->alloc) {
		for(i=0;i<holders->len;i++)
			hkl_holder_release(&holders->holders[i]);
		free(holders->holders);
		hkl_holders_init(holders);
	}
}

void hkl_holders_free(HklHolders *holders)
{
	hkl_holders_release(holders);
	free(holders);
}

HklHolder* hkl_holders_add_holder(HklHolders *holders)
{
	hkl_holders_grow(holders, 1);
	return &holders->holders[holders->len++];
}
