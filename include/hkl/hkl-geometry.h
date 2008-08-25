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
	HklAxis **axes;
	size_t axes_len;
	HklHolder *holders;
	size_t holders_len;
};


/* the holder part */

extern void hkl_holder_init(HklHolder *self, HklGeometry *geometry);
extern int hkl_holder_init_copy(HklHolder *self, HklGeometry *geometry,
		HklHolder const *holder);
extern void hkl_holder_release_memory(HklHolder *self);

extern HklAxis *hkl_holder_add_rotation_axis(HklHolder *self,
		char const *name, double x, double y, double z);

extern size_t hkl_holder_size(HklHolder const *self);

extern void hkl_holder_update(HklHolder *self);

extern void hkl_holder_apply_to_vector(HklHolder const *self, HklVector *v);

/* the geometry part */

extern HklGeometry *hkl_geometry_new(void);
extern HklGeometry *hkl_geometry_new_copy(HklGeometry const *self);

extern void hkl_geometry_free(HklGeometry *self);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *self);
extern HklHolder *hkl_geometry_get_holder(HklGeometry const *self,
		size_t idx);

extern void hkl_geometry_update(HklGeometry *self);

extern void hkl_geometry_fprintf(FILE *file, HklGeometry const *self);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
