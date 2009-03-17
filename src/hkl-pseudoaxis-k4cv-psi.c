#include <hkl/hkl-pseudoaxis-k4cv.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k4cv_psi_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineModePsi *getset;

	self = hkl_pseudo_axis_engine_psi_new();

	/* psi get/set */
	char const *axes_names_psi[] = {"komega", "kappa", "kphi", "tth"};
	getset = hkl_pseudo_axis_engine_mode_psi_new("psi", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_get_set(self, (HklPseudoAxisEngineMode *)getset);

	hkl_pseudo_axis_engine_select_get_set(self, 0);

	return self;
}
