#include <string.h>

#include "config.h"
#include "new_holder.h"
#include "svector.h"
#include "smatrix.h"
#include "quaternion.h"

/* private hkl_holder part */
void hkl_holder_grow(struct hkl_holder * holder, size_t extra)
{
	if (holder->len + extra <= holder->len)
		die("you want to use way too much memory");
	if (!holder->alloc)
		holder->idx = NULL;
	ALLOC_GROW(holder->idx, holder->len + extra, holder->alloc);
}

void hkl_holder_init(struct hkl_holder * holder, size_t hint)
{
	holder->len = 0;
	holder->alloc = 0;
	if (hint)
		hkl_holder_grow(holder, hint);
}

void hkl_holder_release(struct hkl_holder * holder)
{
	if (holder->alloc) {
		free(holder->idx);
		hkl_holder_init(holder, 0);
	}
}

void hkl_holder_add_idx(struct hkl_holder * holder, size_t const idx)
{
	hkl_holder_grow(holder, 1);
	holder->len++;
	holder->idx[holder->len] = idx;
	holder->dirty = HKL_TRUE;	
}

/* private hkl_holders part */
void hkl_holders_grow(struct hkl_holders * holders, size_t extra)
{
	if (holders->len + extra <= holders->len)
		die("you want to use way too much memory");
	if (!holders->alloc)
		holders->holders = NULL;
	ALLOC_GROW(holders->holders, holders->len + extra, holders->alloc);
}

void hkl_holders_init(struct hkl_holders * holders, size_t hint)
{
	holders->len = 0;
	holders->alloc = 0;
	if (hint)
		hkl_holders_grow(holders, hint);
}

void hkl_holders_release(struct hkl_holders * holders)
{
	size_t i;

	if (holders->alloc) {
		for(i=0;i<holders->len;i++)
			hkl_holder_release(&holders->holders[i]);
		free(holders->holders);
		hkl_holders_init(holders, 0);
	}
}

/* public part */
void hkl_holder_add_rotation_axe(struct hkl_holder * holder, char const * name, struct hkl_svector const * rot_axis)
{
	size_t i;
	size_t idx;
	struct hkl_axis * axis = NULL;

	idx = hkl_axis_get_idx_by_name(holder->axes, name);
	if (idx >= 0) { // found it in the axe list
    		// check that both have the same rotation axe.
    		if (hkl_svector_cmp(&holder->axes->axes[idx].axis, rot_axis)) {
      			// check that the axis is not already in the holder
			for(i=0; i<holder->len; i++)
				if (idx == holder->idx[i])
					die("can not add two times the \"%s\" axis to an holder.", name);
      
			// add 1 element to ther holder
			hkl_holder_add_idx(holder, idx);
		} else
			die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an holder",
				name,
				axis->axis.data[X], axis->axis.data[Y], axis->axis.data[Z],
				rot_axis->data[X], rot_axis->data[Y], rot_axis->data[Z]);
	} else {// not already in the axe list
		// add a rotation axe to the holder->axes list
		hkl_axes_add_rotation(holder->axes, name, rot_axis);
		// set the right index in the holder idx list
		hkl_holder_add_idx(holder, holder->axes->len);
	}
}

struct hkl_holder * hkl_holders_add(struct hkl_holders * holders)
{
	struct hkl_holder * holder;

	hkl_holders_grow(holders, 1);
	holder = &holders->holders[holders->len++];

	return holder;
}
