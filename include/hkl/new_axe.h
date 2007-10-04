#ifndef _NEW_AXE_H
#define _NEW_AXE_H

#include "svector.h"
#include "interval.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

    struct hkl_axis_config
      {
        struct hkl_interval range;
        double current;
        double consign;
      };


    struct hkl_axis
      {
        const char * name;
        struct hkl_svector axis;
        struct hkl_axis_config config;
      };

    struct hkl_axes
    {
      size_t alloc;
      size_t len;
      struct hkl_axis *axes;
    };

  extern struct hkl_axis * hkl_axes_add_rotation(struct hkl_axes * axes, char const * name, struct hkl_svector const * rot_axis);

  extern size_t hkl_axis_get_idx_by_name(struct hkl_axes * axes, char const * name);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _NEW_AXE_H */
