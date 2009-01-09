#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e6c-hkl.h>
#include <hkl/hkl-pseudoaxis-common.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_horizontal(const gsl_vector *x, void *params, gsl_vector *f)
{
	double mu, omega, gamma;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	omega = x_data[1];
	gamma = x_data[4];

	f_data[3] = omega;
	f_data[4] = gamma - 2 * fmod(mu, M_PI);

	return  GSL_SUCCESS;
}

static int bissector_vertical(const gsl_vector *x, void *params, gsl_vector *f)
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

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector_horizontal(HklPseudoAxisEngine *engine,
								   HklGeometry *geometry,
								   HklDetector *detector,
								   HklSample *sample)
{
	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	return hkl_pseudo_axis_engine_solve_function(engine, bissector_horizontal);
}

static int hkl_pseudo_axis_engine_setter_func_bissector_vertical(HklPseudoAxisEngine *engine,
								 HklGeometry *geometry, HklDetector *detector,
								 HklSample *sample)
{
	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	return hkl_pseudo_axis_engine_solve_function(engine, bissector_vertical);
}

/***********************/
/* E6C PseudoAxeEngine */
/***********************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e6c_hkl_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;

	self = hkl_pseudo_axis_engine_new("hkl", 3, "h", "k", "l");
	
	/* set the default range of the pseudo axes */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].config.range.min = -1.;
		self->pseudoAxes[i].config.range.max = 1;
	}

	/* bissector_horizontal */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"bissector_horizontal",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_bissector_horizontal,
		0,
		5, "mu", "omega", "chi", "phi", "gamma");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* bissector_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"bissector_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_bissector_vertical,
		0,
		4, "omega", "chi", "phi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_omega_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_omega_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_hkl,
		0,
		3, "chi", "phi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_chi_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_chi_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_hkl,
		0,
		3, "omega", "phi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_phi_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_phi_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_hkl,
		0,
		3, "omega", "chi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* lifting_detector */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"lifting_detector",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_hkl,
		0,
		3, "phi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	return self;
}
