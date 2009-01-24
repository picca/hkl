#ifndef __HKL_PSEUDOAXIS_COMMON_H__
#define __HKL_PSEUDOAXIS_COMMON_H__

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

extern int hkl_pseudo_axis_engine_init_func(HklPseudoAxisEngine *self,
					    HklGeometry *geometry,
					    HklDetector const *detector,
					    HklSample const *sample);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_H__ */
