#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-k4cv.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k4cv_psi_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSetPsi *getset;

	self = hkl_pseudo_axis_engine_psi_new();

	/* psi get/set */
	char const *axes_names_psi[] = {"komega", "kappa", "kphi", "tth"};
	getset = hkl_pseudo_axis_engine_get_set_psi_new("psi", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_get_set(self, (HklPseudoAxisEngineGetSet *)getset);

	return self;
}
