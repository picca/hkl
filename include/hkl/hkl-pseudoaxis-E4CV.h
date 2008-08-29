#ifndef __HKL_PSEUDOAXIS_E4CV_H__
#define __HKL_PSEUDOAXIS_E4CV_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>
#include <hkl/hkl-pseudoaxis-common.h>

HKL_BEGIN_DECLS

static int E4CV_bissector_f(const gsl_vector *x, void *params, gsl_vector *f)
{
	double omega, tth;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	RUBh_minus_Q(x_data, params, f_data);

	omega = gsl_sf_angle_restrict_symm(x_data[0]);
	tth = gsl_sf_angle_restrict_symm(x_data[3]);

	//f_data[3] = tth - 2 * fmod(omega,M_PI);
	f_data[3] = tth - 2 * omega;

	return  GSL_SUCCESS;
}

static int E4CV_constant_omega_f(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->function->parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[0];

	return  GSL_SUCCESS;
}

static int E4CV_constant_chi_f(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->function->parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[1];

	return  GSL_SUCCESS;
}

static int E4CV_constant_phi_f(const gsl_vector *x, void *params,
		gsl_vector *f)
{
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->function->parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[2];

	return  GSL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_E4CV_HKL(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineFunc *function;
	HklParameter parameter = {NULL, {-M_PI, M_PI}, 0., 0};

	self = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	/* bissector */
	function = hkl_pseudo_axis_engine_func_new(
		"bissector",				/* name */
		1, E4CV_bissector_f,			/* functions */
		0,					/* parameters */
		4, "omega", "chi", "phi", "tth");	/* related axes */
	hkl_pseudoAxisEngine_add_function(self, function);

	/* constant_omega */
	parameter.name = "omega";
	function = hkl_pseudo_axis_engine_func_new(
		"constant_omega",
		1, E4CV_constant_omega_f,
		1, &parameter,
		4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_function(self, function);

	/* constant_chi */
	parameter.name = "chi";
	function = hkl_pseudo_axis_engine_func_new(
		"constant_chi",
		1, E4CV_constant_chi_f,
		1, &parameter,
		4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_function(self, function);

	/* constant_phi */
	parameter.name = "phi";
	function = hkl_pseudo_axis_engine_func_new(
		"constant_phi",
		1, E4CV_constant_phi_f,
		1, &parameter,
		4, "omega", "chi", "phi", "tth");
	hkl_pseudoAxisEngine_add_function(self, function);

	return self;
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_E4CV_H__ */
