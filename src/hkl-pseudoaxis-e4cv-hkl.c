#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-common-hkl.h>

/*******/
/* hkl */
/*******/

static int bissector(const gsl_vector *x, void *params, gsl_vector *f)
{
	double omega, tth;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	omega = x_data[0];
	tth = x_data[3];

	f_data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

static int hkl_pseudo_axis_engine_setter_func_bissector(HklPseudoAxisEngine *engine,
							HklGeometry *geometry,
							HklDetector *detector,
							HklSample *sample)
{
	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	return hkl_pseudo_axis_engine_solve_function(engine, bissector);
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4cv_hkl_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;

	self = hkl_pseudo_axis_engine_new("hkl", 3, "h", "k", "l");

	/* set the default range of the pseudo axes */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].parent.range.min = -1.;
		self->pseudoAxes[i].parent.range.max = 1;
	}

	/* hkl get/set bissector */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"bissector",
		NULL,
		hkl_pseudo_axis_engine_get_set_get_hkl_real,
		hkl_pseudo_axis_engine_setter_func_bissector,
		0,
		4, "omega", "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_omega */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_omega",
		NULL,
		hkl_pseudo_axis_engine_get_set_get_hkl_real,
		hkl_pseudo_axis_engine_get_set_set_hkl_real,
		0,
		3, "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_chi */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_chi",
		NULL,
		hkl_pseudo_axis_engine_get_set_get_hkl_real,
		hkl_pseudo_axis_engine_get_set_set_hkl_real,
		0,
		3, "omega", "phi", "tth");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_phi */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_phi",
		NULL,
		hkl_pseudo_axis_engine_get_set_get_hkl_real,
		hkl_pseudo_axis_engine_get_set_set_hkl_real,
		0,
		3, "omega", "chi", "tth");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* double_diffraction */
	HklParameter h2 = {"h2", {-1., 1.}, 1., 0};
	HklParameter k2 = {"k2", {-1, 1}, 1., 0};
	HklParameter l2 = {"l2", {-1, 1}, 1., 0};

	getset = hkl_pseudo_axis_engine_get_set_new(
		"double_diffraction",
		NULL,
		hkl_pseudo_axis_engine_get_set_get_hkl_real,
		hkl_pseudo_axis_engine_get_set_set_double_diffraction_real,
		3, &h2, &k2, &l2,
		4, "omega", "chi", "phi", "tth");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	return self;
}
