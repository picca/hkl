#include <math.h>
#include <string.h>

#include "config.h"
#include "new_axe.h"

static struct hkl_axis_config hkl_axis_config_default = {{-M_PI, M_PI}, 0., 0.};

void hkl_axis_init_rotation(struct hkl_axis * axis, char const * name, struct hkl_svector const * axis_v)
{
  axis->name = name;
  axis->axis = *axis_v;
  axis->config = hkl_axis_config_default; 
}

void hkl_axes_grow(struct hkl_axes * axes, size_t extra)
{
  if (axes->len + extra <= axes->len)
      die("you want to use way too much memory");
  if (!axes->alloc)
      axes->axes = NULL;
  ALLOC_GROW(axes->axes, axes->len + extra, axes->alloc);
}

void hkl_axes_init(struct hkl_axes * axes, size_t hint)
{
  axes->len = 0;
  axes->alloc = 0;
  if (hint)
      hkl_axes_grow(axes, hint);
}

void hkl_axes_release(struct hkl_axes * axes)
{
  if (axes->alloc)
  {
    free(axes->axes);
    hkl_axes_init(axes, 0);
  }
}

/** @brief add a rotation axis to the axe list */
struct hkl_axis * hkl_axes_add_rotation(struct hkl_axes * axes, char const * name, struct hkl_svector const * rot_axis)
{
  size_t i;
  struct hkl_axis * axis = NULL;

  // check if an axis with the same name is in the axis list.
  for(i=0;i<axes->len;i++)
    if (strcmp(axes->axes[i].name, name) == 0)
      die("can not add two axis with the same name \"%s\" in the axe list.", name);

  // grow the axes list of 1
  hkl_axes_grow(axes, 1);
  axes->len++;
    
  // set the right parameters
  axis = &axes->axes[axes->len];
  hkl_axis_init_rotation(axis, name, rot_axis);

  return axis;
}

size_t hkl_axis_get_idx_by_name(struct hkl_axes * axes, char const * name)
{
  size_t i = -1;
  for(i=0;i<axes->len;i++)
    if (strcmp(axes->axes[i].name, name) == 0)
      break;

  return i;
}
