#ifndef __HKL_PSEUDOAXIS_E4CV_H__
#define __HKL_PSEUDOAXIS_E4CV_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

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
	double p0 = engine->function.parameters[0].value;

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
	double p0 = engine->function.parameters[0].value;

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
	double p0 = engine->function.parameters[0].value;

	RUBh_minus_Q(x_data, params, f_data);

	f_data[3] = p0 - x_data[2];

	return  GSL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_E4CV_HKL(void)
{
	static char const *pseudo_names[] = {"h", "k", "l"};
	static char const *axes_names[] = {"omega", "chi", "phi", "tth"};

	static HklPseudoAxisEngineFunction f_bissector[] = {
		E4CV_bissector_f,
	};

	static HklPseudoAxisEngineFunction f_constant_omega[] = {
		E4CV_constant_omega_f,
	};
	static HklParameter p_constant_omega[] = {
		{"omega", {-M_PI, M_PI}, 0, 0},
	};

	static HklPseudoAxisEngineFunction f_constant_chi[] = {
		E4CV_constant_chi_f,
	};
	static HklParameter p_constant_chi[] = {
		{"chi", {-M_PI, M_PI}, 0, 0},
	};

	static HklPseudoAxisEngineFunction f_constant_phi[] = {
		E4CV_constant_phi_f,
	};
	static HklParameter p_constant_phi[] = {
		{"phi", {-M_PI, M_PI}, 0, 0},
	};

	static HklPseudoAxisEngineFunc functions[] = {
		{
			.name		= "bissector",
			.f		= f_bissector,
			.f_len		= 1,
			.parameters	= NULL,
			.parameters_len	= 0,
		},
		{
			.name		= "constant_omega",
			.f		= f_constant_omega,
			.f_len		= 1,
			.parameters	= p_constant_omega,
			.parameters_len	= 1,
		},
		{
			.name		= "constant_chi",
			.f		= f_constant_chi,
			.f_len		= 1,
			.parameters	= p_constant_chi,
			.parameters_len	= 1,
		},
		{
			.name		= "constant_phi",
			.f		= f_constant_phi,
			.f_len		= 1,
			.parameters	= p_constant_phi,
			.parameters_len	= 1,
		},
	};
	static HklPseudoAxisEngineConfig config = {
		.name 			= "hkl",
		.pseudo_names 		= pseudo_names,
		.pseudo_names_len	= 3,
		.axes_names		= axes_names,
		.axes_names_len		= 4,
		.functions		= functions,
		.functions_len		= 4,
	};

	return hkl_pseudoAxisEngine_new(&config);
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_E4CV_H__ */
