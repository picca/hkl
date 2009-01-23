#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e6c.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e6c_psi_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSetPsi *getset;

	self = hkl_pseudo_axis_engine_psi_new();

	/* psi get/set */
	char const *axes_names_psi[] = {"omega", "chi", "phi", "delta"};
	getset = hkl_pseudo_axis_engine_get_set_psi_new("psi_vertical", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_get_set(self, (HklPseudoAxisEngineGetSet *)getset);

	hkl_pseudo_axis_engine_select_get_set(self, 0);

	return self;
}
