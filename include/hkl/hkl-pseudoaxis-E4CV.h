#ifndef __HKL_PSEUDOAXIS_E4CV_H__
#define __HKL_PSEUDOAXIS_E4CV_H__

#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

static int common_hkl_part(const gsl_vector *x, void *params, gsl_vector *f)
{
	HklVector Hkl;
	HklVector ki, dQ;
	HklPseudoAxisEngine *engine;
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	unsigned int i;

	engine = params;
	H = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 0);
	K = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 1);
	L = hkl_pseudoAxisEngine_get_pseudoAxis(engine, 2);

	// update the workspace from x;
	for(i=0; i<engine->related_axes_idx->size ; ++i) {
		HklAxis *axis;
		unsigned int idx;
		HklAxisConfig config;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->geom, idx);
		hkl_axis_get_config(axis, &config);
		config.value = gsl_vector_get(x, i);

		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->geom);

	hkl_vector_set(&Hkl, H->config.value, K->config.value,
			L->config.value);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = hkl_geometry_get_holder(engine->geom, 0);
	hkl_matrix_times_vector(engine->sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, holder->q);

	// kf - ki = Q
	hkl_source_get_ki(engine->geom->source, &ki);
	hkl_detector_get_kf(engine->det, engine->geom, &dQ);
	hkl_vector_minus_vector(&dQ, &ki);

	hkl_vector_minus_vector(&dQ, &Hkl);

	gsl_vector_set (f, 0, dQ.data[0]);
	gsl_vector_set (f, 1, dQ.data[1]);
	gsl_vector_set (f, 2, dQ.data[2]);

	return GSL_SUCCESS;
}

static int E4CV_bissector(const gsl_vector *x, void *params, gsl_vector *f)
{
	double omega, tth;

	common_hkl_part(x, params, f);

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

	common_hkl_part(x, params, f);

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
