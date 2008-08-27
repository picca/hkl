#include <gsl/gsl_sf_trig.h>
#include <hkl/hkl-pseudoaxis.h>

/* private */

static void clear_geometries(HklPseudoAxisEngine *engine)
{
	size_t i;

	if (engine->geometries_len)
		for(i=0; i<engine->geometries_len; ++i)
			hkl_geometry_free(engine->geometries[i]);
	free(engine->geometries);
	engine->geometries = NULL;
	engine->geometries_len = 0;
}

static void add_geometry(HklPseudoAxisEngine *engine, gsl_vector const *x)
{
	size_t i, n;
	HklGeometry *geometry;
	double const *x_data = gsl_vector_const_ptr(x, 0);

	for(i=0; i<engine->axes_len; ++i)
		engine->axes[i]->config.value = x_data[i];

	geometry = hkl_geometry_new_copy(engine->geometry);
	n = engine->geometries_len++;
	engine->geometries = realloc(engine->geometries,
			engine->geometries_len * sizeof(HklGeometry*));
	engine->geometries[n] = geometry;
}

static int find_geometry(HklPseudoAxisEngine *self,
		gsl_multiroot_function *f)
{
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_vector *x;
	double *x_data;
	size_t iter = 0;
	int status;
	size_t i;
	double d;

	// get the starting point from the geometry
	// must be put in the auto_set method
	x = gsl_vector_alloc(self->axes_len);
	x_data = gsl_vector_ptr(x, 0);
	for(i=0; i<self->axes_len; ++i)
		x_data[i] = self->axes[i]->config.value;

	// Initialize method 
	T = gsl_multiroot_fsolver_hybrids;
	s = gsl_multiroot_fsolver_alloc (T, self->axes_len);
	gsl_multiroot_fsolver_set (s, f, x);

	// iterate to find the solution
	do {
		++iter;
		status = gsl_multiroot_fsolver_iterate(s);
		if (status || iter % 1000 == 0) {
			// Restart from another point.
			for(i=0; i<self->axes_len; ++i) {
				d = (double)rand() / RAND_MAX * 180. / M_PI;
				gsl_vector_set(x, i, d);
			}
			gsl_multiroot_fsolver_set(s, f, x);
			status = gsl_multiroot_fsolver_iterate(s);
		}
		status = gsl_multiroot_test_residual (s->f, HKL_EPSILON);
	} while (status == GSL_CONTINUE && iter < 10000);
	if (status == GSL_CONTINUE)
		return HKL_FAIL;

	// set the geometry from the gsl_vector
	// in a futur version the geometry must contain a gsl_vector
	// to avoid this.
	for(i=0; i<self->axes_len; ++i) {
		HklAxis *axis = self->axes[i];
		x_data = gsl_vector_ptr(s->x, 0);
		axis->config.value = gsl_sf_angle_restrict_pos(x_data[i]);
		axis->config.dirty = 1;
	}
	hkl_geometry_update(self->geometry);

	// release memory
	gsl_vector_free(x);
	gsl_multiroot_fsolver_free(s);

	return HKL_SUCCESS;
}

/** 
 * @brief given a vector of angles change the sector of thoses angles
 * 
 * @param x The vector of angles to change.
 * @param sector the sector vector operation.
 *
 * 0 -> no change
 * 1 -> pi - angle
 * 2 -> pi + angle
 * 3 -> -angle
 */
static void change_sector(gsl_vector *x, double const *x0,
		int const *sector, size_t n)
{
	unsigned int i;

	for(i=0; i<n; ++i) {
		double value = x0[i];
		switch (sector[i]) {
			case 0:
				break;
			case 1:
				value = M_PI - value;
				break;
			case 2:
				value = M_PI + value;
				break;
			case 3:
				value = -value;
				break;
		}
		gsl_vector_set(x, i, value);
	}
}

/** 
 * @brief Test if an angle combination is compatible with a pseudoAxisEngine
 * 
 * @param x The vector of angles to test.
 * @param engine The pseudoAxeEngine used for the test.
 */
static int test_sector(gsl_vector const *x,
		gsl_multiroot_function const *function)
{
	int ko;
	size_t i;
	gsl_vector *f;

	f = gsl_vector_alloc(x->size); 

	function->f(x, function->params, f);
	ko = 0;
	for(i=0;i<f->size; ++i) {
		if (fabs(gsl_vector_get(f, i)) > HKL_EPSILON) {
			ko = 1;
			gsl_vector_free(f);
			return HKL_FAIL;
		}
	}
	if (!ko) {
		gsl_matrix *J;

		J = gsl_matrix_alloc(x->size, f->size);

		//gsl_multiroot_fdjacobian(&function->f, x, f,
		//		GSL_SQRT_DBL_EPSILON, J);
		/*
		fprintf(stdout, "\n");
		hkl_geometry_fprintf(stdout, engine->geom);
		fprintf(stdout, "\n ");
		for(i=0;i<x->size;++i)
			fprintf(stdout, " %d", gsl_vector_int_get(p, i));
		fprintf(stdout, "   ");
		for(i=0;i<x->size;++i)
			fprintf(stdout, " %f", gsl_vector_get(f, i));
		fprintf(stdout, "\n");
		for(i=0;i<state->n;++i) {
			fprintf(stdout, "\n   ");
			for(j=0;j<state->n;++j)
				fprintf(stdout, " %f", gsl_matrix_get(J, i, j));
		}
		fprintf(stdout, "\n");
		*/
		gsl_matrix_free(J);
	}
	gsl_vector_free(f);
	return HKL_SUCCESS;
}

/* 
 * @brief compute the permutation and test its validity.
 * 
 * @param axes_len number of axes
 * @param op_len number of operation per axes. (4 for now)
 * @param p The vector containing the current permutation.
 * @param axes_idx The index of the axes we are permution.
 * @param op the current operation to set.
 * @param f The function for the validity test.
 * @param x0 The starting point of all geometry permutations.
 */
static void perm_r(size_t axes_len, int op_max,
		int *p, int axes_idx, int op,
		gsl_multiroot_function *f,
		double *x0)
{
	int i;

	p[axes_idx++] = op;
	if (axes_idx == axes_len) {
		//fprintf(stdout, "%d %d %d %d\n", p[0], p[1], p[2], p[3]);
		gsl_vector *x = gsl_vector_alloc(axes_len);
		change_sector(x, x0, p, axes_len);
		if (!test_sector(x, f))
			add_geometry(f->params, x);
		gsl_vector_free(x);
	} else
		for (i=0; i<op_max; ++i)
			perm_r(axes_len, op_max, p, axes_idx, i, f, x0);
}

/* public */

HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(HklPseudoAxisEngineConfig *config)
{
	size_t i;
	HklPseudoAxisEngine *self = NULL;

	self = malloc(sizeof(*self));
	if (!self)
		die("Can not allocate memory for an HklPseudoAxisEngine");

	self->config = *config;
	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;
	self->axes = NULL;
	self->axes_len = 0;

	// create the pseudoAxes from the config
	self->pseudoAxes_len = config->pseudo_names_len;
	self->pseudoAxes = malloc(self->pseudoAxes_len * sizeof(HklPseudoAxis));
	for(i=0; i<self->pseudoAxes_len; ++i) {
		self->pseudoAxes[i].name = config->pseudo_names[i];
		self->pseudoAxes[i].engine = self;
	}

	self->geometries = NULL;
	self->geometries_len = 0;

	return self;
}

void hkl_pseudoAxisEngine_set(HklPseudoAxisEngine *self, size_t idx_f,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample)

{
	size_t i;

	if (self->geometry)
		hkl_geometry_free(self->geometry);
	self->geometry = hkl_geometry_new_copy(geometry);
	if(!self->geometry)
		die("Can not allocate memory for an HklGeometry");
	hkl_geometry_update(self->geometry);

	self->detector = detector;
	self->sample = sample;

	self->function = self->config.functions[idx_f];

	// fill the axes member from the config
	if (self->axes_len)
		free(self->axes), self->axes = NULL, self->axes_len = 0;
	self->axes_len = self->config.axes_names_len;
	self->axes = malloc(self->axes_len * sizeof(HklAxis *));
	for(i=0; i<self->axes_len; ++i)
		self->axes[i] = hkl_geometry_get_axis_by_name(self->geometry,
				self->config.axes_names[i]);
}

void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *self)
{
	if (self->geometry)
		hkl_geometry_free(self->geometry);
	if(self->axes_len)
		free(self->axes), self->axes = NULL, self->axes_len = 0;
	if (self->pseudoAxes_len)
		free(self->pseudoAxes), self->pseudoAxes = NULL, self->pseudoAxes_len = 0;
	clear_geometries(self);
	free(self);
}

int RUBh_minus_Q(gsl_vector const *x, void *params, gsl_vector *f)
{
	HklVector Hkl;
	HklVector ki, dQ;
	HklPseudoAxisEngine *engine;
	HklPseudoAxis *H, *K, *L;
	HklHolder *holder;
	double const *x_data = gsl_vector_const_ptr(x, 0);
	double *f_data = gsl_vector_ptr(f, 0);
	size_t i;

	engine = params;
	H = &engine->pseudoAxes[0];
	K = &engine->pseudoAxes[1];
	L = &engine->pseudoAxes[2];

	// update the workspace from x;
	for(i=0; i<engine->axes_len ; ++i) {
		HklAxis *axis = engine->axes[i];
		axis->config.value = x_data[i];
		axis->config.dirty = 1;
	}
	hkl_geometry_update(engine->geometry);

	hkl_vector_init(&Hkl, H->config.value, K->config.value,
			L->config.value);

	// R * UB * h = Q
	// for now the 0 holder is the sample holder.
	holder = &engine->geometry->holders[0];
	hkl_matrix_times_vector(&engine->sample->UB, &Hkl);
	hkl_vector_rotated_quaternion(&Hkl, &holder->q);

	// kf - ki = Q
	hkl_source_compute_ki(&engine->geometry->source, &ki);
	hkl_detector_compute_kf(engine->detector, engine->geometry, &dQ);
	hkl_vector_minus_vector(&dQ, &ki);

	hkl_vector_minus_vector(&dQ, &Hkl);

	f_data[0] = dQ.data[0];
	f_data[1] = dQ.data[1];
	f_data[2] = dQ.data[2];

	return GSL_SUCCESS;
}

int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *self)
{
	HklHolder *holder;
	HklMatrix RUB;
	HklVector hkl, ki, Q;

	// update the geometry internals
	hkl_geometry_update(self->geometry);

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = &self->geometry->holders[0];
	hkl_quaternion_to_smatrix(&holder->q, &RUB);
	hkl_matrix_times_smatrix(&RUB, &self->sample->UB);

	// kf - ki = Q
	hkl_source_compute_ki(&self->geometry->source, &ki);
	hkl_detector_compute_kf(self->detector, self->geometry, &Q);
	hkl_vector_minus_vector(&Q, &ki);

	hkl_matrix_solve(&RUB, &hkl, &Q);

	// update the pseudoAxes current and consign parts
	self->pseudoAxes[0].config.value = hkl.data[0];
	self->pseudoAxes[1].config.value = hkl.data[1];
	self->pseudoAxes[2].config.value = hkl.data[2];

	return HKL_SUCCESS;
}

int hkl_pseudoAxisEngine_to_geometry(HklPseudoAxisEngine *self)
{
	size_t idx, i;
	size_t n = self->axes_len;
	int *p;
	int res = 0;

	clear_geometries(self);
	for(idx=0; idx<self->function.f_len; ++idx) {
		gsl_multiroot_function f = {self->function.f[idx], self->axes_len, self};
		int tmp = !find_geometry(self, &f);
		res |= tmp;
		if (tmp) {
			p = calloc(n , sizeof(int));
			double *x0 = malloc(n * sizeof(double));
			for(i=0; i<n; ++i)
				x0[i] = self->axes[i]->config.value;

			for (i=0; i<n; ++i)
				perm_r(n, 4, p, 0, i, &f, x0);

			free(x0);
			free(p);
		}
	}
	return res;
}
