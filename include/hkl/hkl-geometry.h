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
typedef struct _HklGeometryList HklGeometryList;

struct _HklHolder {
	HklGeometry *geometry;
	HKL_LIST(size_t, idx);
	HklQuaternion q;
};

struct _HklGeometry
{
	char const *name;
	HklSource source;
	HKL_LIST(HklAxis, axes);
	HKL_LIST(HklHolder, holders);
};

struct _HklGeometryList
{
	HKL_LIST(HklGeometry *,geometries);
};

/*************/
/* HklHolder */
/*************/

extern HklAxis *hkl_holder_add_rotation_axis(HklHolder *self,
					     char const *name, double x, double y, double z);

/***************/
/* HklGeometry */
/***************/

extern HklGeometry *hkl_geometry_new(void);
extern HklGeometry *hkl_geometry_new_copy(HklGeometry const *self);

extern void hkl_geometry_free(HklGeometry *self);

extern void hkl_geometry_init_geometry(HklGeometry *self,
				       HklGeometry const *src);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *self);

extern void hkl_geometry_update(HklGeometry *self);

extern HklAxis *hkl_geometry_get_axis_by_name(HklGeometry *self,
					      char const *name);

extern void hkl_geometry_randomize(HklGeometry *self);

extern int hkl_geometry_set_values_v(HklGeometry *self,
				     size_t len, ...);

extern double hkl_geometry_distance(HklGeometry *self, HklGeometry *geom);

extern void hkl_geometry_fprintf(FILE *file, HklGeometry const *self);

/*******************/
/* HklGeometryList */
/*******************/

extern HklGeometryList *hkl_geometry_list_new(void);

extern void hkl_geometry_list_free(HklGeometryList *self);

extern void hkl_geometry_list_add(HklGeometryList *self, HklGeometry *geometry);

extern void hkl_geometry_list_reset(HklGeometryList *self);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
