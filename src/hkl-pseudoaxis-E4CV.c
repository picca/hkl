#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-E4CV.h>
#include <hkl/hkl-pseudoaxis-common.h>

/***********************/
/* numerical functions */
/***********************/

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

static int constant_omega(const gsl_vector *x, void *params,
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

static int constant_chi(const gsl_vector *x, void *params,
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

static int constant_phi(const gsl_vector *x, void *params,
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

static int hkl_pseudo_axis_engine_setter_func_bissector(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, bissector);
}

static int hkl_pseudo_axis_engine_setter_func_constant_omega(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, constant_omega);
}

static int hkl_pseudo_axis_engine_setter_func_constant_chi(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, constant_chi);
}

static int hkl_pseudo_axis_engine_setter_func_constant_phi(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	return hkl_pseudoAxeEngine_solve_function(engine, constant_phi);
}

/************************/
/* E4CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_E4CV_HKL(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;
	HklParameter parameter = {NULL, {-M_PI, M_PI}, 0., 0};

	self = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	/* hkl get/set bissector */
	getset = hkl_pseudo_axis_engine_get_set_new(
			"bissector",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_bissector,
			0,
			4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_omega */
	parameter.name = "omega";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_omega",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_omega,
			1, &parameter,
			4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_chi */
	parameter.name = "chi";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_chi",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_chi,
			1, &parameter,
			4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_phi */
	parameter.name = "phi";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_phi",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_phi,
			1, &parameter,
			4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	return self;
}
