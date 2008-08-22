#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis.h>

typedef struct
{
	size_t n;
}
auto_state_t;

//forward declaration
static int _auto_to_geometry(void *vstate, HklPseudoAxisEngineFunc *function,
		HklPseudoAxisEngine *engine);

static int auto_alloc(void *vstate, size_t n)
{
	return HKL_SUCCESS;
}

static int auto_set(void *vstate, HklPseudoAxisEngineFunc *function,
		HklPseudoAxisEngine *engine)
{
	auto_state_t *state;

	state = vstate;
	state->n = function->f.n;

	return HKL_SUCCESS;
}

static int auto_to_geometry(void *vstate, HklPseudoAxisEngineFunc *function,
		HklPseudoAxisEngine *engine)
{
	_auto_to_geometry(vstate, function, engine);

	return HKL_SUCCESS;
}

static int auto_to_pseudoAxes(void *vstate,
		HklPseudoAxisEngineFunc *function,
		HklPseudoAxisEngine *engine)
{
	HklHolder *holder;
	HklMatrix RUB;
	HklVector hkl, ki, Q;
	HklPseudoAxis *H, *K, *L;

	// update the geometry internals
	hkl_geometry_update(engine->geom);

	// R * UB
	// for now the 0 holder is the sample holder.
	holder = &engine->geom->holders[0];
	hkl_quaternion_to_smatrix(&holder->q, &RUB);
	hkl_matrix_times_smatrix(&RUB, &engine->sample->UB);

	// kf - ki = Q
	hkl_source_compute_ki(&engine->geom->source, &ki);
	hkl_detector_get_kf(engine->det, engine->geom, &Q);
	hkl_vector_minus_vector(&Q, &ki);

	hkl_matrix_solve(&RUB, &hkl, &Q);

	// update the pseudoAxes current and consign parts
	H = hkl_list_get_by_idx(engine->pseudoAxes, 0);
	K = hkl_list_get_by_idx(engine->pseudoAxes, 1);
	L = hkl_list_get_by_idx(engine->pseudoAxes, 2);
	H->config.value = hkl.data[0];
	K->config.value = hkl.data[1];
	L->config.value = hkl.data[2];

	return HKL_SUCCESS;
}

static void auto_free(void *state)
{
}

static int _auto_to_geometry(void *vstate, HklPseudoAxisEngineFunc *function,
		HklPseudoAxisEngine *engine)
{
	auto_state_t *state;
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_vector *x;
	size_t iter = 0;
	int status;
	size_t i;
	unsigned int  idx;
	double d;

	state = vstate;

	// get the starting point from the geometry
	// must be put in the auto_set method
	x = gsl_vector_alloc(state->n);
	for(i=0; i<state->n; ++i) {
		HklAxis const *axis;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->geom, idx);
		gsl_vector_set(x, i, axis->config.value);
	}

	// Initialize method 
	T = gsl_multiroot_fsolver_hybrids;
	s = gsl_multiroot_fsolver_alloc (T, state->n);
	gsl_multiroot_fsolver_set (s, &function->f, x);

	// iterate to find the solution
	do {
		++iter;
		status = gsl_multiroot_fsolver_iterate(s);
		if (status || iter % 1000 == 0) {
			// Restart from another point.
			for(i=0; i<state->n; ++i) {
				d = (double)rand() / RAND_MAX * 180. / M_PI;
				gsl_vector_set(x, i, d);
			}
			gsl_multiroot_fsolver_set(s, &function->f, x);
			status = gsl_multiroot_fsolver_iterate(s);
		}
		status = gsl_multiroot_test_residual (s->f, HKL_EPSILON);
	} while (status == GSL_CONTINUE && iter < 10000);
	if (status == GSL_CONTINUE){
		fprintf(stdout, "toto %d\n", iter);
		return GSL_ENOMEM;
	}

	// set the geometry from the gsl_vector
	// in a futur version the geometry must contain a gsl_vector
	// to avoid this.
	for(i=0; i<state->n; ++i) {
		HklAxisConfig config;
		HklAxis *axis;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis(engine->geom, idx);
		hkl_axis_get_config(axis, &config);
		d = gsl_vector_get(s->x, i);
		gsl_sf_angle_restrict_pos_e(&d);
		config.value = d;
		hkl_axis_set_config(axis, &config);
	}
	hkl_geometry_update(engine->geom);

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
static void change_sector(gsl_vector *x, gsl_vector_uint const *sector)
{
	unsigned int i;

	for(i=0; i<x->size; ++i) {
		double value;

		value = gsl_vector_get(x, i);
		switch (gsl_vector_uint_get(sector, i)) {
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
static void test_sector(gsl_vector const *x,
		HklPseudoAxisEngineFunc const *function,
		HklPseudoAxisEngine *engine)
{
	int ko;
	size_t i;
	gsl_vector *f;

	f = gsl_vector_alloc(x->size); 

	(function->f.f)(x, engine, f);
	ko = 0;
	for(i=0;i<f->size; ++i) {
		if (fabs(gsl_vector_get(f, i)) > HKL_EPSILON) {
			ko = 1;
			break;
		}
	}
	if (!ko) {
		gsl_matrix *J;

		J = gsl_matrix_alloc(x->size, f->size);
		gsl_multiroot_fdjacobian(&function->f, x, f,
				GSL_SQRT_DBL_EPSILON, J);
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
}

/* 
 * @brief given a ref geometry populate a vector for a specific engine.
 * 
 * @param x The vector to populate with the right axes values.
 * @param engine The engine use to take the right axes from geom.
 * @param geom The geom use to extract the angles into x.
 */
static void get_axes_as_gsl_vector(gsl_vector *x,
		HklPseudoAxisEngine *engine, HklGeometry const *geom)
{
	size_t i;

	for(i=0; i<x->size; ++i) {
		HklAxis const *axis;
		size_t idx;

		idx = gsl_vector_uint_get(engine->related_axes_idx, i);
		axis = hkl_geometry_get_axis_const(geom, idx);
		gsl_vector_set(x, i, axis->config.value);
	}
}

/* 
 * @brief compute the permutation and test its validity.
 * 
 * @param n number of axes
 * @param k number of operation per axes. (4 for now)
 * @param p The vector containing the current permutation.
 * @param z The index of the axes we are permution.
 * @param x the current operation to set.
 * @param engine The engine for the validity test.
 * @param geom The starting point of all geometry permutations.
 */
static void perm_r(const int n, const int k,
		gsl_vector_uint *p, int z, int x,
		HklPseudoAxisEngine *engine,
		HklGeometry const *geom)
{
	int i;

	gsl_vector_uint_set(p, z++, x);
	if (z == k) {
		gsl_vector *x;

		x = gsl_vector_alloc(n);
		get_axes_as_gsl_vector(x, engine, geom);
		change_sector(x, p);
		test_sector(x, engine->function, engine);
		gsl_vector_free(x);
	} else
		for (i=0; i<n; ++i)
			perm_r(n, k, p, z, i, engine, geom);
}

/*
 * @brief The method use to compute all equivalent geometries.
 * 
 * @param vstate the state of the auto pseudoAxeEngine.
 * @param engine The engine to use.
 * 
 * @return 
 */
static int auto_equiv_geometries(void *vstate,
		HklPseudoAxisEngineFunc *function,
		HklPseudoAxisEngine *engine)
{
	size_t i, n;
	gsl_vector_uint *p;
	HklGeometry const *geom;

	geom = hkl_geometry_new_copy(engine->geom);

	n = engine->related_axes_idx->size;
	p = gsl_vector_uint_calloc(n);

	for (i=0; i<n; ++i)
		perm_r(n, 4, p, 0, i, engine, geom);

	gsl_vector_uint_free(p);

	return GSL_SUCCESS;
}

static HklPseudoAxisEngineType auto_type =
{
	"auto",
	sizeof(auto_state_t),
	&auto_alloc,
	&auto_set,
	&auto_to_geometry,
	&auto_to_pseudoAxes,
	&auto_equiv_geometries,
	&auto_free
};

const HklPseudoAxisEngineType *hkl_pseudoAxisEngine_type_auto = &auto_type;
