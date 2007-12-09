#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <hkl/hklsource.h>
#include <hkl/hklholders.h>

HKL_BEGIN_DECLS

typedef struct _HklGeometry HklGeometry;

struct _HklGeometry {
	HklSource source;
	HklHolders holders;
};

extern void hkl_geometry_init(HklGeometry *geometry);

extern HklHolder * hkl_geometry_add_holder(HklGeometry *geometry);

extern void hkl_geometry_release(HklGeometry *geometry);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
