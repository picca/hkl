#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <hkl/hkl-source.h>
#include <hkl/hkl-geometry.h>
#include <hkl/hkl-list.h>
#include <hkl/hkl-quaternion.h>
#include <hkl/hkl-axis.h>

HKL_BEGIN_DECLS

typedef struct _HklHolder HklHolder;
typedef struct _HklGeometry HklGeometry;

struct _HklHolder {
	HklGeometry *geometry;
	HklAxis **axes;
	size_t axes_len;
	HklQuaternion q;
};

struct _HklGeometry
{
	HklSource source;
	HklList *axes;
	HklHolder *holders;
	size_t holders_len;
};


/* the holder part */

extern void hkl_holder_init(HklHolder *self, HklGeometry *geometry);
extern int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
		HklHolder const *holder);
extern void hkl_holder_release_memory(HklHolder *holder);

extern HklAxis *hkl_holder_add_rotation_axis(HklHolder *holder,
		char const *name, double x, double y, double z);

extern size_t hkl_holder_size(HklHolder const *holder);

extern void hkl_holder_update(HklHolder *holder);

extern void hkl_holder_apply_to_vector(HklHolder const *holder, HklVector *v);

/* the geometry part */

extern HklGeometry *hkl_geometry_new(void);
extern HklGeometry *hkl_geometry_new_copy(HklGeometry const *g);

extern void hkl_geometry_free(HklGeometry *g);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *g);
extern HklHolder *hkl_geometry_get_holder(HklGeometry const *g, size_t idx);

extern HklAxis *hkl_geometry_get_axis(HklGeometry *g, size_t idx);
extern HklAxis const *hkl_geometry_get_axis_const(HklGeometry const *g,
		size_t idx);

extern void hkl_geometry_update(HklGeometry *g);

extern void hkl_geometry_fprintf(FILE *file, HklGeometry const *g);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
