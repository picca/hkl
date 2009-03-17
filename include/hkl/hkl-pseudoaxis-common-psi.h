#ifndef __HKL_PSEUDOAXIS_COMMON_PSI_H__
#define __HKL_PSEUDOAXIS_COMMON_PSI_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxisEngineModePsi HklPseudoAxisEngineModePsi;

struct _HklPseudoAxisEngineModePsi
{
	HklPseudoAxisEngineMode parent;
	HklVector Q0;
	HklVector hkl0;
};

extern HklPseudoAxisEngineModePsi *hkl_pseudo_axis_engine_get_set_psi_new(char const *name,
									    size_t axes_names_len,
									    char const *axes_names[]);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_psi_new(void);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_COMMON_PSI_H__ */
