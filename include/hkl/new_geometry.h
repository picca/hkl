#ifndef _NEW_GEOMETRY_H
#define _NEW_GEOMETRY_H

#include <string.h>

#include "config.h"
#include "source.h"
#include "holder.h"
#include "axis.h"
#include "svector.h"
#include "smatrix.h"
#include "quaternion.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

struct hkl_geometry_config {
	struct hkl_source source;
	struct hkl_axes_config * axes;
};

struct hkl_geometry {
	struct hkl_source source;
	struct hkl_axes * axes;
	struct hkl_holders * holders;
};

extern void init_geometry(struct hkl_geometry * geom);

extern void free_geometry(struct hkl_geometry * geom);

extern struct hkl_geometry_config * hkl_geometry_get_config(struct hkl_geometry const * geometry);

extern void hkl_geometry_set_config(struct hkl_geometry * geometry, struct hkl_geometry_config const * config);

extern void free_geometry_config(struct hkl_geometry_config * config);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _NEW_GEOMETRY_H */
