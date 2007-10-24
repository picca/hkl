#ifndef _NEW_DIFFRACTOMETER_H
#define _NEW_DIFFRACTOMETER_H

#include <stdarg.h>

#include "config.h"
#include "new_geometry.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

struct hkl_diffractometer_config {
	struct hkl_geometry_config * geometry;
}

struct hkl_diffractometer {
	struct hkl_geometry * geometry;
	//struct hkl_samples * samples;
	//struct hkl_pseudoAxes * pseudoAxes;
};

enum diff_type
{
	DIFF_TYPE_2C,
	DIFF_TYPE_E4CV,
	DIFF_TYPE_E6C,
	DIFF_TYPE_K4CV,
	DIFF_TYPE_K6C,
}

static struct hkl_diffractometer * create_diffractometer(diff_type type, ...);

static void free_diffractometer(struct hkl_diffractometer * diff);

extern hkl_diffractometer_config * hkl_diffractometer_get_config(struct hkl_diffractometer * diff);

extern void hkl_diffractometer_set_config(struct hkl_diffractometer * diff, struct hkl_diffractometer_config const * config);

extern void free_diffractometer_config(struct hkl_diffractometer_config * config);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _NEW_DIFFRACTOMETER_H */
