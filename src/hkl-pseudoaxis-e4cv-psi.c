#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-e4cv-psi.h>
#include <hkl/hkl-pseudoaxis-common.h>

typedef struct _HklPseudoAxisEngineGetSetPsi HklPseudoAxisEngineGetSetPsi;

struct _HklPseudoAxisEngineGetSetPsi
{
	HklPseudoAxisEngineGetSet parent;
	HklVector Q0;
	HklVector hkl0;
};

static int psi(const gsl_vector *x, void *params, gsl_vector *f)
{

	HklVector dhkl0, hkl1;
	HklVector ki, kf, Q, n;
	HklMatrix RUB;
	HklPseudoAxisEngine *engine;
	HklPseudoAxisEngineGetSetPsi *getsetpsi;
	HklPseudoAxis *psi;
	HklHolder *holder;
	size_t i;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);

	engine = params;
	getsetpsi = (HklPseudoAxisEngineGetSetPsi *)engine->getset;
	psi = &engine->pseudoAxes[0];

	// update the workspace from x;
	for(i=0; i<engine->axes_len; ++i) {
		HklAxis *axis = engine->axes[i];
		axis->config.value = x_data[i];
		axis->config.dirty = 1;
	}
	hkl_geometry_update(engine->geometry);

	// kf - ki = Q
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);
	if (hkl_vector_is_null(&Q)){
		f_data[0] = dhkl0.data[0];
		f_data[1] = dhkl0.data[1];
		f_data[2] = dhkl0.data[2];
		f_data[3] = 1;
	}else{
		// R * UB
		// for now the 0 holder is the sample holder.
		holder = &engine->geometry->holders[0];
		hkl_quaternion_to_smatrix(&holder->q, &RUB);
		hkl_matrix_times_smatrix(&RUB, &engine->sample->UB);

		// compute dhkl0
		hkl_matrix_solve(&RUB, &dhkl0, &Q);
		hkl_vector_minus_vector(&dhkl0, &getsetpsi->hkl0);

		// compute the intersection of the plan P(kf, ki) and PQ (normal Q)
		/* 
		 * now that dhkl0 have been computed we can use a
		 * normalized Q to compute n and psi
		 */ 
		hkl_vector_normalize(&Q);
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		// compute hkl1 in the laboratory referentiel
		// for now the 0 holder is the sample holder.
		hkl1.data[0] = engine->getset->parameters[0].value;
		hkl1.data[1] = engine->getset->parameters[1].value;
		hkl1.data[2] = engine->getset->parameters[2].value;
		hkl_vector_times_smatrix(&hkl1, &engine->sample->UB);
		hkl_vector_rotated_quaternion(&hkl1, &engine->geometry->holders[0].q);
	
		// project hkl1 on the plan of normal Q
		hkl_vector_project_on_plan(&hkl1, &Q);
		if (hkl_vector_is_null(&hkl1)){ // hkl1 colinear with Q
			f_data[0] = dhkl0.data[0];
			f_data[1] = dhkl0.data[1];
			f_data[2] = dhkl0.data[2];
			f_data[3] = 1;
		}else{
			f_data[0] = dhkl0.data[0];
			f_data[1] = dhkl0.data[1];
			f_data[2] = dhkl0.data[2];
			f_data[3] = psi->config.value - hkl_vector_oriented_angle(&n, &hkl1, &Q);
		}
	}
	return GSL_SUCCESS;
}

static int init_psi_real(HklPseudoAxisEngine *engine,
			 HklGeometry *geometry,
			 HklDetector const *detector,
			 HklSample const *sample)
{
	int status = HKL_SUCCESS;
	HklVector ki;
	HklVector kf;
	HklMatrix RUB;
	HklPseudoAxisEngineGetSetPsi *self;
	HklPseudoAxisEngineGetSet *base;
	HklHolder *holder;
	
	status = hkl_pseudo_axis_engine_init_func(engine, geometry, detector, sample);
	if (status == HKL_FAIL)
		return status;

	self = (HklPseudoAxisEngineGetSetPsi *)engine->getset;

	// update the geometry internals
	hkl_geometry_update(geometry);

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = &geometry->holders[0];
	hkl_quaternion_to_smatrix(&holder->q, &RUB);
	hkl_matrix_times_smatrix(&RUB, &sample->UB);

	// kf - ki = Q0
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &self->Q0);
	hkl_vector_minus_vector(&self->Q0, &ki);
	if (hkl_vector_is_null(&self->Q0))
		status = HKL_FAIL;
	else
		// compute hkl0
		hkl_matrix_solve(&RUB, &self->hkl0, &self->Q0);

	return status;
}

static int get_psi_real(HklPseudoAxisEngine *engine,
			HklGeometry *geometry,
			HklDetector const *detector,
			HklSample const *sample)
{
	int status = HKL_SUCCESS;

	if (!engine || !engine->getset || !geometry || !detector || !sample){
		status = HKL_FAIL;
		return status;
	}

	double psi;
	HklVector ki;
	HklVector kf;
	HklVector Q;
	HklVector hkl1;
	HklVector n;
	HklPseudoAxisEngineGetSetPsi *self;
	HklPseudoAxisEngineGetSet *base;

	self = (HklPseudoAxisEngineGetSetPsi *)engine->getset;
	base = engine->getset;

	// get kf, ki and Q
	hkl_source_compute_ki(&geometry->source, &ki);
	hkl_detector_compute_kf(detector, geometry, &kf);
	Q = kf;
	hkl_vector_minus_vector(&Q, &ki);
	if (hkl_vector_is_null(&Q))
		status = HKL_FAIL;
	else{
		hkl_vector_normalize(&Q); // needed for a problem of precision

		// compute the intersection of the plan P(kf, ki) and PQ (normal Q)
		n = kf;
		hkl_vector_vectorial_product(&n, &ki);
		hkl_vector_vectorial_product(&n, &Q);

		// compute hkl1 in the laboratory referentiel
		// the geometry was already updated in the detector compute kf
		// for now the 0 holder is the sample holder.
		hkl1.data[0] = base->parameters[0].value;
		hkl1.data[1] = base->parameters[1].value;
		hkl1.data[2] = base->parameters[2].value;
		hkl_vector_times_smatrix(&hkl1, &sample->UB);
		hkl_vector_rotated_quaternion(&hkl1, &geometry->holders[0].q);
	
		// project hkl1 on the plan of normal Q
		hkl_vector_project_on_plan(&hkl1, &Q);
	
		if (hkl_vector_is_null(&hkl1))
			status = HKL_FAIL;
		else
			// compute the angle beetween hkl1 and n
			engine->pseudoAxes[0].config.value = hkl_vector_oriented_angle(&n, &hkl1, &Q);
	}

	return status;
}

static int set_psi_real(HklPseudoAxisEngine *engine,
			HklGeometry *geometry,
			HklDetector *detector,
			HklSample *sample)
{
	hkl_pseudoAxeEngine_prepare_internal(engine, geometry, detector,
					     sample);

	return hkl_pseudoAxeEngine_solve_function(engine, psi);
}

static HklPseudoAxisEngineGetSetPsi *hkl_pseudo_axis_engine_get_set_psi_new(void)
{
	HklPseudoAxisEngineGetSetPsi *self;
	char const *name = "psi";
	char const *parameters_names[] = {"h1", "k1", "l1"};
	char const *axes_names[] = {"omega", "chi", "phi", "tth"};


	self = calloc(1, sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngineGetSetPsi");

	// the base constructor;
	hkl_pseudo_axis_engine_get_set_init(&self->parent,
					    name,
					    init_psi_real,
					    get_psi_real,
					    set_psi_real,
					    3, parameters_names,
					    4, axes_names);

	self->parent.parameters[0].value = 1;
	self->parent.parameters[0].range.min = -1;
	self->parent.parameters[0].range.max = 1;
	self->parent.parameters[0].not_to_fit = HKL_FALSE;

	self->parent.parameters[1].value = 0;
	self->parent.parameters[1].range.min = -1;
	self->parent.parameters[1].range.max = 1;
	self->parent.parameters[1].not_to_fit = HKL_FALSE;

	self->parent.parameters[2].value = 0;
	self->parent.parameters[2].range.min = -1;
	self->parent.parameters[2].range.max = 1;
	self->parent.parameters[2].not_to_fit = HKL_FALSE;

	return self;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_e4cv_psi_new(void)
{
	size_t i;
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSetPsi *getset;

	self = hkl_pseudoAxisEngine_new("psi", 1, "psi");

	/* set the default range of the pseudo axes */
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].config.range.min = -M_PI;
		self->pseudoAxes[i].config.range.max = M_PI;
	}

	/* psi get/set */
	getset = hkl_pseudo_axis_engine_get_set_psi_new();
	hkl_pseudoAxisEngine_add_get_set(self, (HklPseudoAxisEngineGetSet *)getset);

	return self;
}
