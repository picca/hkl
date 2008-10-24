#ifndef __HKL_PSEUDOAXIS_AUTO_H__
#define __HKL_PSEUDOAXIS_AUTO_H__

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

typedef int (* HklPseudoAxisEngineFunction) (const gsl_vector *x, void *params, gsl_vector *f);

extern int hkl_pseudoAxeEngine_solve_function(HklPseudoAxisEngine *self,
					      HklPseudoAxisEngineFunction function);
HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_AUTO_H__ */
