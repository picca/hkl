#ifndef __HKL_PSEUDOAXIS_COMMON_PSI_H__
#define __HKL_PSEUDOAXIS_COMMON_PSI_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxisEngineGetSetPsi HklPseudoAxisEngineGetSetPsi;

struct _HklPseudoAxisEngineGetSetPsi
{
	HklPseudoAxisEngineGetSet parent;
	HklVector Q0;
	HklVector hkl0;
};

extern int hkl_pseudo_axis_engine_get_set_init_psi_real(HklPseudoAxisEngine *engine,
							HklGeometry *geometry,
							HklDetector const *detector,
							HklSample const *sample);

extern int hkl_pseudo_axis_engine_get_set_get_psi_real(HklPseudoAxisEngine *engine,
						       HklGeometry *geometry,
						       HklDetector const *detector,
						       HklSample const *sample);

extern int hkl_pseudo_axis_engine_get_set_set_psi_real(HklPseudoAxisEngine *engine,
						       HklGeometry *geometry,
						       HklDetector *detector,
						       HklSample *sample);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_PSI_H__ */
