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

extern void hkl_source_init(HklSource *source, double wave_length, double x, double y, double z);

extern int hkl_source_cmp(HklSource const *s1, HklSource const *s2);

extern void hkl_source_get_ki(HklSource const *source, HklVector *ki);

HKL_END_DECLS

#endif /* __HKL_SOURCE_H__ */
