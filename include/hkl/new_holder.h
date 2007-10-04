#ifndef _NEW_HOLDER_H
#define _NEW_HOLDER_H

#include "new_axe.h"
#include "quaternion.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

  /* forward declaration */
  struct hkl_svector;

  struct hkl_holder
  {
    struct hkl_axes * axes;
    unsigned int len;
    unsigned int alloc;
    unsigned int *idx;

    int dirty;
    struct hkl_quaternion _q;
  };

  struct hkl_holders
  {
    struct hkl_axes axes;
    unsigned int len;
    unsigned int alloc;
    struct hkl_holder *holders;
  };

  void hkl_holder_add_rotation_axe(struct hkl_holder * holder, char const * name, struct hkl_svector const * rot_axis);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _NEW_HOLDER_H */
