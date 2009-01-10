#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-k6c.h>
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

static int constant_kphi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
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
	gamma = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = omega;

	return  GSL_SUCCESS;
}

static int constant_kphi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
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
	gamma = x_data[3];

	omega = komega + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = omega;

	return  GSL_SUCCESS;
}

static int constant_phi_h_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, kphi;
	double omega, phi, p;
	size_t i;
	HklPseudoAxisEngine *engine;
//	double p0 = engine->getset->parameters[0].value;
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
	kphi = x_data[3];
	gamma = x_data[4];

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));

	omega = komega + p - M_PI_2;
	phi = kphi + p + M_PI_2;

	f_data[3] = omega;
	f_data[4] = phi;
	
	return  GSL_SUCCESS;
}

static int constant_phi_h_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double gamma, mu, komega, kappa, kphi;
	double omega, phi, p;
	size_t i;
	HklPseudoAxisEngine *engine;
//	double p0 = engine->getset->parameters[0].value;
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
	kphi = x_data[3];
	gamma = x_data[4];

	p = atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD));

	omega = komega + p + M_PI_2;
	phi = kphi + p - M_PI_2;

	f_data[3] = omega;
	f_data[4] = phi;
	
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

static int constant_chi_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, chi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];

	chi = 2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	f_data[3] = p0 - chi;

	return  GSL_SUCCESS;
}

static int constant_chi_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, chi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];

	chi = -2 * asin(sin(kappa/2.) * sin(50 * HKL_DEGTORAD));

	f_data[3] = p0 - chi;

	return  GSL_SUCCESS;
}

static int constant_phi_v_f1(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, kphi, phi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];
	kphi = x_data[2];

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) + M_PI_2;

	f_data[3] = p0 - phi;

	return  GSL_SUCCESS;
}

static int constant_phi_v_f2(const gsl_vector *x, void *params, gsl_vector *f)
{
	double kappa, kphi, phi;
	size_t i;
	HklPseudoAxisEngine *engine = params;
	double p0 = engine->getset->parameters[0].value;
	double const *x_data = x->data;
	double *f_data = f->data;

	for(i=0; i<x->size;++i)
		if (gsl_isnan(x_data[i]))
			return GSL_ENOMEM;

	RUBh_minus_Q(x_data, params, f_data);

	kappa = x_data[1];
	kphi = x_data[2];

	phi = kphi + atan(tan(kappa/2.)*cos(50 * HKL_DEGTORAD)) - M_PI_2;

	f_data[3] = p0 - phi;

	return  GSL_SUCCESS;
}

/*********************/
/* Getter and Setter */
/*********************/

static int hkl_pseudo_axis_engine_setter_func_bissector_h(HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_phi_h(HklPseudoAxisEngine *engine,
							      HklGeometry *geometry,
							      HklDetector *detector,
							      HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_kphi_h(HklPseudoAxisEngine *engine,
							      HklGeometry *geometry,
							      HklDetector *detector,
							      HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_kphi_h_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_kphi_h_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_bissector_v(HklPseudoAxisEngine *engine,
							  HklGeometry *geometry,
							  HklDetector *detector,
							  HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, bissector_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_omega_v(HklPseudoAxisEngine *engine,
							       HklGeometry *geometry,
							       HklDetector *detector,
							       HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_omega_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_omega_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_chi_v(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector *detector,
							     HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_chi_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_chi_v_f2);

	return res;
}

static int hkl_pseudo_axis_engine_setter_func_constant_phi_v(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector *detector,
							     HklSample *sample)
{
	int res = 0;

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector,
						sample);

	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_v_f1);
	res |= hkl_pseudo_axis_engine_solve_function(engine, constant_phi_v_f2);

	return res;
}

/************************/
/* K6CV PseudoAxeEngine */
/************************/

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k6c_hkl_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;
	HklParameter parameter = {NULL, {-M_PI, M_PI}, 0., 0};

	self = hkl_pseudo_axis_engine_new("hkl", 3, "h", "k", "l");

	/* set the default range of the pseudo axes */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].config.range.min = -1.;
		self->pseudoAxes[i].config.range.max = 1;
	}

	/* bissector_horizontal */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"bissector_horizontal",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_bissector_h,
		0,
		5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_phi_horizontal */
	parameter.name = "phi";
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_phi_horizontal",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_constant_phi_h,
		1, &parameter,
		5, "mu", "komega", "kappa", "kphi", "gamma");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* horizontal kphi constant */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_kphi_horizontal",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_constant_kphi_h,
		0,
		4, "mu", "komega", "kappa", "gamma");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* bissector_vertical */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"bissector_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_bissector_v,
		0,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_omega_vertical */
	parameter.name = "omega";
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_omega_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_constant_omega_v,
		1, &parameter,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_chi_vertical */
	parameter.name = "chi";
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_chi_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_constant_chi_v,
		1, &parameter,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* constant_phi_vertical */
	parameter.name = "phi";
	getset = hkl_pseudo_axis_engine_get_set_new(
		"constant_phi_vertical",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_constant_phi_v,
		1, &parameter,
		4, "komega", "kappa", "kphi", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	/* lifting_detector */
	getset = hkl_pseudo_axis_engine_get_set_new(
		"lifting_detector",
		NULL,
		hkl_pseudo_axis_engine_getter_func_hkl,
		hkl_pseudo_axis_engine_setter_func_hkl,
		0,
		3, "kphi", "gamma", "delta");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	return self;
}
