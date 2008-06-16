#ifndef __HKL_PSEUDOAXIS_E4CV_H__
#define __HKL_PSEUDOAXIS_E4CV_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

static int E4CV_bissector(const gsl_vector *x, void *params, gsl_vector *f)
{
	double omega, tth;

	RUBh_minus_Q(x, params, f);

	omega = gsl_sf_angle_restrict_pos(gsl_vector_get(x, 0));
	tth = gsl_sf_angle_restrict_pos(gsl_vector_get(x, 3));

	gsl_vector_set (f, 3, tth - 2 * fmod(omega,M_PI));

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

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_E4CV_H__ */
