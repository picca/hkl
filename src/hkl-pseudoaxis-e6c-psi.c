#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e6c-psi.h>
#include <hkl/hkl-pseudoaxis-common-psi.h>

static HklPseudoAxisEngineGetSetPsi *hkl_pseudo_axis_engine_get_set_e6c_psi_e4cv_new(void)
{
	HklPseudoAxisEngineGetSetPsi *self;
	char const *name = "psi e4cv";
	char const *parameters_names[] = {"h1", "k1", "l1"};
	char const *axes_names[] = {"omega", "chi", "phi", "delta"};


	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineGetSetPsi");

	// the base constructor;
	hkl_pseudo_axis_engine_get_set_init(&self->parent,
					    name,
					    hkl_pseudo_axis_engine_get_set_init_psi_real,
					    hkl_pseudo_axis_engine_get_set_get_psi_real,
					    hkl_pseudo_axis_engine_get_set_set_psi_real,
					    3, parameters_names,
					    4, axes_names);

	self->parent.parameters[0].value = 1;
	self->parent.parameters[0].range.min = -1;
	self->parent.parameters[0].range.max = 1;
	self->parent.parameters[0].not_to_fit = HKL_FALSE;

	self->parent.parameters[1].value = 0;
	self->parent.parameters[1].range.min = -1;
	self->parent.parameters[1].range.max = 1;
	self->parent.parameters[1].not_to_fit = HKL_FALSE;

	self->parent.parameters[2].value = 0;
	self->parent.parameters[2].range.min = -1;
	self->parent.parameters[2].range.max = 1;
	self->parent.parameters[2].not_to_fit = HKL_FALSE;

	return self;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e6c_psi_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSetPsi *getset;

	self = hkl_pseudoAxisEngine_new("psi", 1, "psi");

	/* set the default range of the pseudo axes */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].config.range.min = -M_PI;
		self->pseudoAxes[i].config.range.max = M_PI;
	}

	/* psi get/set */
	getset = hkl_pseudo_axis_engine_get_set_e6c_psi_e4cv_new();
	hkl_pseudoAxisEngine_add_get_set(self, (HklPseudoAxisEngineGetSet *)getset);

	return self;
}
