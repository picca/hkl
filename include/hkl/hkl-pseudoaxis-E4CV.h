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

	RUBh_minus_Q(x, params, f);

	omega = gsl_sf_angle_restrict_pos(x_data[0]);
	tth = gsl_sf_angle_restrict_pos(x_data[3]);

	f_data[3] = tth - 2 * fmod(omega,M_PI);

	return  GSL_SUCCESS;
}

static int K4CV_bissector(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, tth, kappa, kphi, omega;
	size_t i;
	HklPseudoAxisEngine *engine;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(gsl_vector_get(x, i)))
			return GSL_ENOMEM;
	//gsl_vector_fprintf(stdout, f, "%f");

	RUBh_minus_Q(x, params, f);

	komega = gsl_sf_angle_restrict_symm(gsl_vector_get(x, 0));
	kappa = gsl_sf_angle_restrict_symm(gsl_vector_get(x, 1));
	kphi = gsl_sf_angle_restrict_symm(gsl_vector_get(x, 2));
	tth = gsl_sf_angle_restrict_symm(gsl_vector_get(x, 3));

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;
	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;
	gsl_sf_angle_restrict_symm_e(&omega);
	//gsl_vector_set (f, 3, tth - 2 * fmod(omega,M_PI));
	gsl_vector_set (f, 3, tth - 2 *omega);

	return  GSL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_E4CV_HKL(void)
{
	static char const *pseudo_names[] = {"h", "k", "l"};
	static char const *axes_names[] = {"omega", "chi", "phi", "tth"};
	static HklPseudoAxisEngineFunction f[] = {
		E4CV_bissector_f
	};
	static HklPseudoAxisEngineFunc functions[] = {
		{
			.f		= f,
			.f_len		= 1,
			.parameters	= NULL,
			.parameters_len	= 0,
		}
	};
	static HklPseudoAxisEngineConfig config = {
		.name 			= "hkl",
		.pseudo_names 		= pseudo_names,
		.pseudo_names_len	= 3,
		.axes_names		= axes_names,
		.axes_names_len		= 4,
		.functions		= functions,
		.functions_len		= 1,
	};

	return hkl_pseudoAxisEngine_new(&config);
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_E4CV_H__ */
