#ifndef __HKL_GEOMETRY_H__
#define __HKL_GEOMETRY_H__

#include <hkl/hkl-source.h>
#include <hkl/hkl-holder.h>

HKL_BEGIN_DECLS

typedef struct _HklGeometry HklGeometry;

struct _HklGeometry
{
	HklSource *source;
	HklList *axes;
	HklList *holders;
};

extern HklGeometry *hkl_geometry_new(void);
extern HklGeometry *hkl_geometry_new_copy(HklGeometry const *g);

extern void hkl_geometry_free(HklGeometry *g);

extern HklHolder *hkl_geometry_add_holder(HklGeometry *g);
extern HklHolder *hkl_geometry_get_holder(HklGeometry const *g, size_t idx);

extern HklAxis *hkl_geometry_get_axis(HklGeometry *g, size_t idx);

extern void hkl_geometry_update(HklGeometry *g);

extern void hkl_geometry_fprintf(FILE *file, HklGeometry const *g);

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_H__ */
