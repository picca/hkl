#include <string.h>

#include "config.h"
#include "new_holder.h"
#include "svector.h"
#include "smatrix.h"
#include "quaternion.h"

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
  if (holder->alloc)
  {
    free(holder->idx);
    hkl_holder_init(holder, 0);
  }
}

void hkl_holder_add_rotation_axe(struct hkl_holder * holder, char const * name, struct hkl_svector const * rot_axis)
{
  size_t i;
  size_t idx;
  struct hkl_axis * axis = NULL;

  idx = hkl_axis_get_idx_by_name(holder->axes, name);
  if (idx >= 0) // found it in the axe list
  {
    // check that both have the same rotation axe.
    if (hkl_svector_cmp(&holder->axes->axes[idx].axis, rot_axis))
    {
      // check that the axis is not already in the holder
      for(i=0; i<holder->len; i++)
        if (idx == holder->idx[i])
          die("can not add two times the \"%s\" axis to an holder.", name);
      
      // add 1 element to ther holder
      hkl_holder_grow(holder, 1);
      holder->len++;
      holder->idx[holder->len] = idx;
      holder->dirty = HKL_TRUE;
    }
    else
      die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an holder",
          name,
          axis->axis.data[X], axis->axis.data[Y], axis->axis.data[Z],
          rot_axis->data[X], rot_axis->data[Y], rot_axis->data[Z]);
  }
  else // not already in the axe list
  {
      // create the rotation axis
      hkl_axis_add_rotation(holder->axes, name, rot_axis);
      // add it to the holder
      hkl_holder_grow(holder, 1);
      holder->len++;
      holder->idx[holder->len] = holder->axes->len;
      holder->dirty = HKL_TRUE;
  }
}
