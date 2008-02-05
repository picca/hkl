#include <hkl/hkl-holders.h>

/* public */
HklHolders *hkl_holders_new(void)
{
	HklHolders *holders = malloc(sizeof(*holders));
	if (!holders)
		die("Cannot allocate memory for an HklHolders");
	
	holders->axes = hkl_list_new();
	holders->holders = hkl_list_new();

	return holders;
}

HklHolders *hkl_holders_new_copy(HklHolders const *src)
{
	HklHolders *copy;
	unsigned int i;

	copy = hkl_holders_new();

	// copy the axes
	copy->axes = hkl_list_new();
	for(i=0; i<src->axes->len; ++i) {
		HklAxis *axis = hkl_axis_new_copy(src->axes->list[i]);
		hkl_list_append(copy->axes, axis);
	}

	// copy the holders
	copy->holders = hkl_list_new();
	for(i=0; i<src->holders->len; ++i) {
		HklHolder *holder;
		
		holder = hkl_holder_new_copy(src->holders->list[i], copy->axes);
		hkl_list_append(copy->holders, holder);
	}

	return copy;
}

void hkl_holders_free(HklHolders *holders)
{
	unsigned int i;

	for(i=0; i<holders->axes->len; ++i)
		hkl_axis_free(holders->axes->list[i]);
	hkl_list_free(holders->axes);

	for(i=0; i<holders->holders->len; ++i)
		hkl_holder_free(holders->holders->list[i]);
	hkl_list_free(holders->holders);

	free(holders);
}

HklHolder *hkl_holders_add_holder(HklHolders *holders)
{
	HklHolder *holder = hkl_holder_new(holders->axes);
	hkl_list_append(holders->holders, holder);

	return holder;
}
