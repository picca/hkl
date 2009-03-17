#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4cv_psi_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineModePsi *mode;

	self = hkl_pseudo_axis_engine_psi_new();

	/* psi get/set */
	char const *axes_names_psi[] = {"omega", "chi", "phi", "tth"};
	mode = hkl_pseudo_axis_engine_mode_psi_new("psi", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_get_set(self, (HklPseudoAxisEngineMode *)mode);

	hkl_pseudo_axis_engine_select_get_set(self, 0);

	return self;
}
