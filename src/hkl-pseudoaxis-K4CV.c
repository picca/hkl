#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-K4CV.h>
#include <hkl/hkl-pseudoaxis-common.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_f1(const gsl_vector *x, void *params, gsl_vector *f)
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
	tth = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = tth - 2 * fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static int bissector_f2(const gsl_vector *x, void *params, gsl_vector *f)
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
	tth = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = tth - 2 * fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	int res = 0;

	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	res |= hkl_pseudoAxeEngine_solve_function(engine, bissector_f1);
	res |= hkl_pseudoAxeEngine_solve_function(engine, bissector_f2);

	return res;
}

/************************/
/* K4CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_K4CV_HKL(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;

	self = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	/* hkl get/set bissector */
	getset = hkl_pseudo_axis_engine_get_set_new(
			"bissector",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_bissector,
			0,
			4, "komega", "kappa", "kphi", "tth");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	return self;
}
