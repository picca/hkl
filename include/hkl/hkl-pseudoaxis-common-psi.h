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

extern HklPseudoAxisEngineGetSetPsi *hkl_pseudo_axis_engine_get_set_psi_new(char const *name,
									    size_t axes_names_len,
									    char const *axes_names[]);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_psi_new(void);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_PSI_H__ */
