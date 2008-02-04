#include <hkl/hkl-holders.h>

/* public */
HklHolders* hkl_holders_new(void)
{
	HklHolders *holders = malloc(sizeof(*holders));
	if (!holders)
		die("Cannot allocate memory for an HklHolders");
	
	holders->axes = hkl_axes_new();
	holders->holders = hkl_list_new();

	return holders;
}

void hkl_holders_free(HklHolders *holders)
{
	size_t i;

	/* release memory */
	for(i=0; i<holders->axes->axes->len; ++i)
		hkl_axis_free(holders->axes->axes->list[i]);
	hkl_axes_free(holders->axes);

	for(i=0; i<holders->holders->len; ++i)
		hkl_holder_free(holders->holders->list[i]);
	hkl_list_free(holders->holders);

	free(holders);
}

HklHolder* hkl_holders_add_holder(HklHolders *holders)
{
	HklHolder *holder = hkl_holder_new(holders->axes);
	hkl_list_append(holders->holders, holder);

	return holder;
}
