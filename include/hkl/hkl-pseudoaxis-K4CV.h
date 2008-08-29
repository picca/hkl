#ifndef __HKL_PSEUDOAXIS_K4CV_H__
#define __HKL_PSEUDOAXIS_K4CV_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>
#include <hkl/hkl-pseudoaxis-common.h>

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
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineFunc *function;

	self = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	/* bissector */
	function = hkl_pseudo_axis_engine_func_new(
			"bissector",
			2, K4CV_bissector_f1, K4CV_bissector_f2,
			0,
			4, "komega", "kappa", "kphi", "tth");
	hkl_pseudoAxisEngine_add_function(self, function);

	return self;
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_E4CV_H__ */
