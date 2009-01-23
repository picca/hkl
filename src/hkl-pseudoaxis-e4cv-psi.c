#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4cv_psi_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSetPsi *getset;

	self = hkl_pseudo_axis_engine_new("psi", 1, "psi");

	// psi
	hkl_parameter_init((HklParameter *)self->pseudoAxes[0],
			   "psi",
			   -M_PI, 0., M_PI,
			   HKL_FALSE,
			   &hkl_unit_angle_rad, &hkl_unit_angle_deg);

	/* psi get/set */
	char const *axes_names_psi[] = {"omega", "chi", "phi", "tth"};
	getset = hkl_pseudo_axis_engine_get_set_psi_new("psi", 4, axes_names_psi);
	hkl_pseudo_axis_engine_add_get_set(self, (HklPseudoAxisEngineGetSet *)getset);

	return self;
}
