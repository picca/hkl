#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <hkl/hkl-source.h>
#include <hkl/hkl-holders.h>

HKL_BEGIN_DECLS

typedef struct _HklGeometry HklGeometry;

struct _HklGeometry {
	HklSource *source;
	HklHolders *holders;
};

extern HklGeometry* hkl_geometry_new(void);

extern void hkl_geometry_free(HklGeometry *geometry);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
