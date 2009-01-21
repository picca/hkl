#ifndef __HKL_SOURCE_H__
#define __HKL_SOURCE_H__

#include <hkl/hkl-vector.h>

HKL_BEGIN_DECLS

#define HKL_SOURCE_DEFAULT_WAVE_LENGTH (1.54)

typedef struct _HklSource HklSource;

struct _HklSource
{
	double wave_length;
	HklVector direction;
};

extern int hkl_source_init(HklSource *self, double wave_length,
			   double x, double y, double z);

extern int hkl_source_cmp(HklSource const *self, HklSource const *s);

extern void hkl_source_compute_ki(HklSource const *self, HklVector *ki);

extern void hkl_source_fprintf(FILE *f, HklSource const *self);

HKL_END_DECLS

#endif /* __HKL_SOURCE_H__ */
