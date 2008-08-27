#ifndef __HKL_PSEUDOAXIS_K4CV_H__
#define __HKL_PSEUDOAXIS_K4CV_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

static int K4CV_bissector_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, tth, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	tth = gsl_sf_angle_restrict_symm(x_data[3]);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;
	omega = gsl_sf_angle_restrict_symm(omega);
	//f_data[3] = tth - 2 * fmod(omega,M_PI);
	f_data[3] = tth - 2*omega;

	return  GSL_SUCCESS;
}

static int K4CV_bissector_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, tth, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	tth = gsl_sf_angle_restrict_symm(x_data[3]);

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;
	omega = gsl_sf_angle_restrict_symm(omega);
	//f_data[3] = tth - 2 * fmod(omega,M_PI);
	f_data[3] = tth - 2*omega;

	return  GSL_SUCCESS;
}

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_K4CV_HKL(void)
{
	static char const *pseudo_names[] = {"h", "k", "l"};
	static char const *axes_names[] = {"komega", "kappa", "kphi", "tth"};
	static HklPseudoAxisEngineFunction f[] = {
		K4CV_bissector_f1, K4CV_bissector_f2
	};
	static HklPseudoAxisEngineFunc functions[] = {
		{"bissector", f, 2, NULL, 0 },
	};
	static HklPseudoAxisEngineConfig config = {
		"hkl",
		pseudo_names, 3,
		axes_names, 4,
		functions, 1,
	};

	return hkl_pseudoAxisEngine_new(&config);
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_E4CV_H__ */
