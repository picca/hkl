#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-E6C.h>
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

static int constant_omega_vertical(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[0];

	return  GSL_SUCCESS;
}

static int constant_chi_vertical(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[1];

	return  GSL_SUCCESS;
}

static int constant_phi_vertical(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[2];

	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector_horizontal(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, bissector_horizontal);
}

static int hkl_pseudo_axis_engine_setter_func_bissector_vertical(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, bissector_vertical);
}

static int hkl_pseudo_axis_engine_setter_func_constant_omega_vertical(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, constant_omega_vertical);
}

static int hkl_pseudo_axis_engine_setter_func_constant_chi_vertical(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, constant_chi_vertical);
}

static int hkl_pseudo_axis_engine_setter_func_constant_phi_vertical(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, constant_phi_vertical);
}

/***********************/
/* E6C PseudoAxeEngine */
/***********************/

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_E6C_HKL(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;
	HklParameter parameter = {NULL, {-M_PI, M_PI}, 0., 0};

	self = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	/* bissector_horizontal */
	getset = hkl_pseudo_axis_engine_get_set_new(
			"bissector_horizontal",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_bissector_horizontal,
			0,
			5, "mu", "omega", "chi", "phi", "gamma");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* bissector_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
			"bissector_vertical",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_bissector_vertical,
			0,
			4, "omega", "chi", "phi", "delta");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_omega_vertical */
	parameter.name = "omega";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_omega_vertical",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_omega_vertical,
			1, &parameter,
			4, "omega", "chi", "phi", "delta");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_chi_vertical */
	parameter.name = "chi";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_chi_vertical",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_chi_vertical,
			1, &parameter,
			4, "omega", "chi", "phi", "delta");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_phi_vertical */
	parameter.name = "phi";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_phi_vertical",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_phi_vertical,
			1, &parameter,
			4, "omega", "chi", "phi", "delta");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	return self;
}
