#include <gsl/gsl_multimin.h>

#include <hkl/hkl-sample.h>
#include <hkl/hkl-matrix.h>

/* private */

/*
static double hkl_sample_mono_crystal_fitness(HklSample const *sample, gsl_vector const *x, void *params)
{
	size_t i, j;
	HklMatrix *U;
	HklMatrix *B;
	HklVector hkl;
	double fitness = 0;
	HklSampleReflection *reflections;

	U = malloc(sizeof(HklMatrix));

	reflections = sample->reflections->reflections;

	for(i=0;i<sample->reflections->len;++i) {
		hkl_matrix_from_euler(U, gsl_vector_get(x, 0), gsl_vector_get(x, 1), gsl_vector_get(x, 2));
		hkl_lattice_get_B(sample->lattice, B);
		hkl_matrix_times_smatrix(U, B);
		hkl = *reflections[i].hkl;
		hkl_matrix_times_svector(U, &hkl);

		for(j=0;j<3;j++)
			fitness += (hkl.data[j] - reflections[i].hkl_phi->data[j]) * (hkl.data[j] - reflections[i].hkl_phi->data[j]);
	}

	free(U);
	return fitness;
}
*/

static void *copy_ref(void const *item)
{
	HklSampleReflection *copy = NULL;
	HklSampleReflection const *src = item;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Can not allocate memory for a HklSampleReflection");

	copy->geometry = hkl_geometry_new_copy(src->geometry);
	copy->detector = hkl_detector_new_copy(src->detector);
	copy->hkl = src->hkl;
	copy->_hkl = src->_hkl;

	return copy;
}

static void free_ref(void *item)
{
	HklSampleReflection *ref = item;
	hkl_geometry_free(ref->geometry);
	hkl_detector_free(ref->detector);
	free(ref);
}

static void compute_UB(HklSample *sample)
{
	HklMatrix B;

	hkl_lattice_get_B(sample->lattice, &B);
	*sample->UB = *sample->U;
	hkl_matrix_times_smatrix(sample->UB, &B);
}

/* public */
HklSample* hkl_sample_new(char const *name, HklSampleType type)
{
	HklSample *sample = NULL;
	sample = malloc(sizeof(*sample));
	if (!sample)
		die("Cannot allocate memory for a Sample");
	sample->name = name;
	sample->type = type;
	sample->lattice = hkl_lattice_new_default();
	sample->U = hkl_matrix_new(1, 0, 0, 0, 1, 0, 0, 0, 1);
	sample->UB = hkl_matrix_new(1, 0, 0, 0, 1, 0, 0, 0, 1);
	compute_UB(sample);
	sample->reflections = hkl_list_new_managed(&copy_ref, &free_ref);

	return sample;
}

HklSample *hkl_sample_new_copy(HklSample const *src)
{
	HklSample *copy;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Cannot allocate memory for a Sample");
	copy->name = src->name;
	copy->type = src->type;
	copy->lattice = hkl_lattice_new_copy(src->lattice);
	copy->U = hkl_matrix_new_copy(src->U);
	copy->UB = hkl_matrix_new_copy(src->UB);
	copy->reflections = hkl_list_new_copy(src->reflections);

	return copy;
}

void hkl_sample_free(HklSample *sample)
{
	hkl_lattice_free(sample->lattice);
	hkl_matrix_free(sample->U);
	hkl_matrix_free(sample->UB);
	hkl_list_free(sample->reflections);
	free(sample);
}

HklSampleReflection *hkl_sample_add_reflection(HklSample *sample,
		HklGeometry *g, HklDetector const *det,
		double h, double k, double l)
{
	HklSampleReflection *ref;
	HklHolder *holder_d;
	HklHolder *holder_s;
	HklVector ki;
	HklQuaternion q;

	if (fabs(h) < HKL_EPSILON
			&& fabs(k) < HKL_EPSILON
			&& fabs(l) < HKL_EPSILON)
		return NULL;

	ref = malloc(sizeof(*ref));
	if (!ref)
		die("Cannot allocate memory for an HklSampleReflection");

	hkl_geometry_update(g);

	ref->geometry = hkl_geometry_new_copy(g);
	ref->detector = hkl_detector_new_copy(det);
	ref->hkl.data[0] = h;
	ref->hkl.data[1] = k;
	ref->hkl.data[2] = l;

	// compute the _hkl using only the axes of the geometry
	holder_d = hkl_list_get_by_idx(ref->geometry->holders, det->idx);
	holder_s = hkl_list_get_by_idx(ref->geometry->holders, 0);

	// compute Q from angles
	hkl_source_get_ki(ref->geometry->source, &ki);
	ref->_hkl = ki;
	hkl_vector_rotated_quaternion(&ref->_hkl, holder_d->q);
	hkl_vector_minus_vector(&ref->_hkl, &ki);

	q = *holder_s->q;
	hkl_quaternion_conjugate(&q);
	hkl_vector_rotated_quaternion(&ref->_hkl, &q);

	hkl_list_append(sample->reflections, ref);

	return ref;
}

HklSampleReflection* hkl_sample_get_reflection(HklSample *sample,
		size_t idx)
{
	return hkl_list_get_by_idx(sample->reflections, idx);
}

int hkl_sample_del_reflection(HklSample *sample, size_t idx)
{
	return hkl_list_del_by_idx(sample->reflections, idx);
}

int hkl_sample_compute_UB_busing_levy(HklSample *sample,
		size_t idx1, size_t idx2)
{
	if (idx1 < sample->reflections->len 
			&& idx2 < sample->reflections->len) {

		HklSampleReflection *r1;
		HklSampleReflection *r2;

		r1 = hkl_list_get_by_idx(sample->reflections, idx1);
		r2 = hkl_list_get_by_idx(sample->reflections, idx2);

		if (!hkl_vector_is_colinear(&r1->hkl, &r2->hkl)) {
			HklVector h1c;
			HklVector h2c;
			HklMatrix B;
			HklMatrix Tc;

			// Compute matrix Tc from r1 and r2.
			h1c = r1->hkl;
			h2c = r2->hkl;
			hkl_lattice_get_B(sample->lattice, &B);
			hkl_matrix_times_vector(&B, &h1c);
			hkl_matrix_times_vector(&B, &h2c);
			hkl_matrix_from_two_vector(&Tc, &h1c, &h2c);
			hkl_matrix_transpose(&Tc);

			// compute U
			hkl_matrix_from_two_vector(sample->U,
					&r1->_hkl, &r2->_hkl);
			hkl_matrix_times_smatrix(sample->U, &Tc);
		} else
			return HKL_FAIL;
	} else
		return HKL_FAIL;

	return HKL_SUCCESS;
}

/*
void hkl_sample_affine(HklSample *sample)
{
	gsl_multimin_fminimizer_type const *T = gsl_multimin_fminimizer_nmsimplex;
	gsl_multimin_fminimizer *s = NULL;
	gsl_vector *ss, *x;
	gsl_multimin_function minex_func;
	size_t iter = 0;
	int status;
	double size;
	size_t nb_params;
	double *params;

	// Starting point
	x = gsl_vector_alloc (3);
	gsl_vector_set (x, 0, 0.);
	gsl_vector_set (x, 1, 0.);
	gsl_vector_set (x, 2, 0.);
	// Set initial step sizes to 1
	ss = gsl_vector_alloc (3);
	gsl_vector_set_all (ss, 1.0 * HKL_DEGTORAD);

	// Initialize method and iterate
	minex_func.n = 3;
	minex_func.f = &hkl_sample_mono_crystal_fitness;
	minex_func.params = NULL;
	s = gsl_multimin_fminimizer_alloc (T, 3);
	gsl_multimin_fminimizer_set (s, &minex_func, x, ss);
	do {
		++iter;
		status = gsl_multimin_fminimizer_iterate(s);
		if (status)
			break;
		size = gsl_multimin_fminimizer_size (s);
		status = gsl_multimin_test_size (size, 1e-2);
		if (status == GSL_SUCCESS) {
			printf ("converged to minimum at\n");
		}
		printf ("%5d %10.3e %10.3e %10.3ef f() = %7.3f size = %.3f\n",
				iter,
				gsl_vector_get (s->x, 0),
				gsl_vector_get (s->x, 1),
				gsl_vector_get (s->x, 2),
				s->fval, size);
	}
	while (status == GSL_CONTINUE && iter < 100);
	gsl_vector_free(x);
	gsl_vector_free(ss);
	gsl_multimin_fminimizer_free(s);
}

void hkl_sample_fprintf(HklSample *sample, FILE *f)
{
	fprintf(f, "\"%s\"\n", sample->name);
}
*/
