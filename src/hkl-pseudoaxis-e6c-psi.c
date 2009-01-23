#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e6c.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e6c_psi_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSetPsi *getset;

	self = hkl_pseudo_axis_engine_new("psi", 1, "psi");

	/* set the default range of the pseudo axes */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].parent.range.min = -M_PI;
		self->pseudoAxes[i].parent.range.max = M_PI;
	}

	/* psi get/set */
	char const *axes_names_psi[] = {"omega", "chi", "phi", "delta"};
	getset = hkl_pseudo_axis_engine_get_set_psi_new("psi_vertical", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_get_set(self, (HklPseudoAxisEngineGetSet *)getset);

	return self;
}
