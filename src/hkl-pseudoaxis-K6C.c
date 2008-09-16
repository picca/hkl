#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-K6C.h>
#include <hkl/hkl-pseudoaxis-common.h>

/***********************/
/* numerical functions */
/***********************/

static int bissector_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = omega;
	f_data[4] = gamma - 2 * fmod(mu, M_PI);

	return  GSL_SUCCESS;
}

static int bissector_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	mu = x_data[0];
	komega = x_data[1];
	kappa = x_data[2];
	gamma = x_data[4];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = omega;
	f_data[4] = gamma - 2 * fmod(mu, M_PI);


	return  GSL_SUCCESS;
}

static int bissector_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, delta, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	delta = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = delta - 2 * fmod(omega, M_PI);

	return  GSL_SUCCESS;
}

static int bissector_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, delta, omega;
	size_t i;
	HklPseudoAxisEngine *engine;
	double const *x_data = x->data;
	double *f_data = f->data;

	engine = params;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];
	delta = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = delta - 2 * fmod(omega, M_PI);


	return  GSL_SUCCESS;
}

static int constant_omega_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = p0 - omega;

	return  GSL_SUCCESS;
}

static int constant_omega_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double komega, kappa, omega;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	komega = x_data[0];
	kappa = x_data[1];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = p0 - omega;

	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector_h(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	int res = 0;

	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	res |= hkl_pseudoAxeEngine_solve_function(engine, bissector_h_f1);
	res |= hkl_pseudoAxeEngine_solve_function(engine, bissector_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_bissector_v(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	int res = 0;

	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	res |= hkl_pseudoAxeEngine_solve_function(engine, bissector_v_f1);
	res |= hkl_pseudoAxeEngine_solve_function(engine, bissector_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_omega_v(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)
{
	int res = 0;

	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
			sample);

	res |= hkl_pseudoAxeEngine_solve_function(engine, constant_omega_v_f1);
	res |= hkl_pseudoAxeEngine_solve_function(engine, constant_omega_v_f2);

	return res;
}

/************************/
/* K6CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new_K6C_HKL(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;
	HklParameter parameter = {NULL, {-M_PI, M_PI}, 0., 0};

	self = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	/* bissector_horizontal */
	getset = hkl_pseudo_axis_engine_get_set_new(
			"bissector_horizontal",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_bissector_h,
			0,
			5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* bissector_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
			"bissector_vertical",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_bissector_v,
			0,
			4, "komega", "kappa", "kphi", "delta");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	/* constant_omega_vertical */
	parameter.name = "omega";
	getset = hkl_pseudo_axis_engine_get_set_new(
			"constant_omega_vertical",
			hkl_pseudo_axis_engine_getter_func_hkl,
			hkl_pseudo_axis_engine_setter_func_constant_omega_v,
			1, &parameter,
			4, "komega", "kappa", "kphi", "delta");
	hkl_pseudoAxisEngine_add_get_set(self, getset);

	return self;
}
