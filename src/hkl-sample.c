#include <gsl/gsl_multimin.h>

#include <hkl/hkl-sample.h>
#include <hkl/hkl-matrix.h>

/* private */

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

static int hkl_sample_compute_UB(HklSample *sample)
{
	HklMatrix B;

	if (hkl_lattice_get_B(sample->lattice, &B))
		return HKL_FAIL;

	sample->UB = sample->U;
	hkl_matrix_times_smatrix(&sample->UB, &B);

	return HKL_SUCCESS;
}

static double mono_crystal_fitness(gsl_vector const *x, void *params)
{
	size_t i, j;
	double fitness;
	double euler_x;
	double euler_y;
	double euler_z;
	HklSample *sample = params;

	euler_x = gsl_vector_get(x, 0);
	euler_y = gsl_vector_get(x, 1);
	euler_z = gsl_vector_get(x, 2);
	sample->lattice->a->value = gsl_vector_get(x, 3);
	sample->lattice->b->value = gsl_vector_get(x, 4);
	sample->lattice->c->value = gsl_vector_get(x, 5);
	sample->lattice->alpha->value = gsl_vector_get(x, 6);
	sample->lattice->beta->value = gsl_vector_get(x, 7);
	sample->lattice->gamma->value = gsl_vector_get(x, 8);
	hkl_matrix_from_euler(&sample->U, euler_x, euler_y, euler_z);
	if (hkl_sample_compute_UB(sample))
		return GSL_NAN;

	fitness = 0.;
	for(i=0; i<sample->reflections->len; ++i) {
		HklVector UBh;
		HklSampleReflection *reflection;

		reflection = hkl_list_get_by_idx(sample->reflections, i);
		UBh = reflection->hkl;
		hkl_matrix_times_vector(&sample->UB, &UBh);

		for(j=0; j<3; ++j) {
			double tmp = UBh.data[j] - reflection->_hkl.data[j];
			fitness += tmp * tmp;
		}
	}
	return fitness;
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
	hkl_matrix_init(&sample->U,1, 0, 0, 0, 1, 0, 0, 0, 1);
	hkl_matrix_init(&sample->UB,1, 0, 0, 0, 1, 0, 0, 0, 1);
	hkl_sample_compute_UB(sample);
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
	copy->U = src->U;
	copy->UB = src->UB;
	copy->reflections = hkl_list_new_copy(src->reflections);

	return copy;
}

void hkl_sample_free(HklSample *sample)
{
	hkl_lattice_free(sample->lattice);
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
	holder_d = &ref->geometry->holders[det->idx];
	holder_s = &ref->geometry->holders[0];

	// compute Q from angles
	hkl_source_compute_ki(&ref->geometry->source, &ki);
	ref->_hkl = ki;
	hkl_vector_rotated_quaternion(&ref->_hkl, &holder_d->q);
	hkl_vector_minus_vector(&ref->_hkl, &ki);

	q = holder_s->q;
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
			hkl_matrix_from_two_vector(&sample->U,
					&r1->_hkl, &r2->_hkl);
			hkl_matrix_times_smatrix(&sample->U, &Tc);
		} else
			return HKL_FAIL;
	} else
		return HKL_FAIL;

	return HKL_SUCCESS;
}

void hkl_sample_affine(HklSample *sample)
{
	gsl_multimin_fminimizer_type const *T = gsl_multimin_fminimizer_nmsimplex;
	gsl_multimin_fminimizer *s = NULL;
	gsl_vector *ss, *x;
	gsl_multimin_function minex_func;
	size_t iter = 0;
	int status;
	double size;

	// Starting point
	x = gsl_vector_alloc (9);
	gsl_vector_set (x, 0, 10 * HKL_DEGTORAD);
	gsl_vector_set (x, 1, 10 * HKL_DEGTORAD);
	gsl_vector_set (x, 2, 10 * HKL_DEGTORAD);
	gsl_vector_set (x, 3, sample->lattice->a->value);
	gsl_vector_set (x, 4, sample->lattice->b->value);
	gsl_vector_set (x, 5, sample->lattice->c->value);
	gsl_vector_set (x, 6, sample->lattice->alpha->value);
	gsl_vector_set (x, 7, sample->lattice->beta->value);
	gsl_vector_set (x, 8, sample->lattice->gamma->value);

	// Set initial step sizes to 1
	ss = gsl_vector_alloc (9);
	gsl_vector_set (ss, 0, 1 * HKL_DEGTORAD);
	gsl_vector_set (ss, 1, 1 * HKL_DEGTORAD);
	gsl_vector_set (ss, 2, 1 * HKL_DEGTORAD);
	gsl_vector_set (ss, 3, !sample->lattice->a->not_to_fit);
	gsl_vector_set (ss, 4, !sample->lattice->b->not_to_fit);
	gsl_vector_set (ss, 5, !sample->lattice->c->not_to_fit);
	gsl_vector_set (ss, 6, !sample->lattice->alpha->not_to_fit);
	gsl_vector_set (ss, 7, !sample->lattice->beta->not_to_fit);
	gsl_vector_set (ss, 8, !sample->lattice->gamma->not_to_fit);

	// Initialize method and iterate
	minex_func.n = 9;
	minex_func.f = &mono_crystal_fitness;
	minex_func.params = sample;
	s = gsl_multimin_fminimizer_alloc (T, 9);
	gsl_set_error_handler_off();
	gsl_multimin_fminimizer_set (s, &minex_func, x, ss);
	do {
		++iter;
		status = gsl_multimin_fminimizer_iterate(s);
		if (status)
			break;
		size = gsl_multimin_fminimizer_size (s);
		status = gsl_multimin_test_size (size, HKL_EPSILON / 2.);
	} while (status == GSL_CONTINUE && iter < 10000);
	gsl_vector_free(x);
	gsl_vector_free(ss);
	gsl_multimin_fminimizer_free(s);
	gsl_set_error_handler (NULL);
}

/*
void hkl_sample_fprintf(HklSample *sample, FILE *f)
{
	fprintf(f, "\"%s\"\n", sample->name);
}
*/
