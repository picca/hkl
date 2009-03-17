#include <hkl/hkl-pseudoaxis-k6c.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k6c_psi_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineModePsi *mode;

	self = hkl_pseudo_axis_engine_psi_new();

	/* psi get/set */
	char const *axes_names_psi[] = {"komega", "kappa", "kphi", "delta"};
	mode = hkl_pseudo_axis_engine_mode_psi_new("psi_vertical", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_mode(self, (HklPseudoAxisEngineMode *)mode);

	hkl_pseudo_axis_engine_select_mode(self, 0);

	return self;
}
