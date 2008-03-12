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
		hkl_smatrix_from_euler(U, gsl_vector_get(x, 0), gsl_vector_get(x, 1), gsl_vector_get(x, 2));
		hkl_lattice_get_B(sample->lattice, B);
		hkl_smatrix_times_smatrix(U, B);
		hkl = *reflections[i].hkl;
		hkl_smatrix_times_svector(U, &hkl);

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

	return copy;
}

static void free_ref(void *item)
{
	HklSampleReflection *ref = item;

	hkl_geometry_free(ref->geometry);
	hkl_detector_free(ref->detector);
	free(ref);
}

/*
static void compute_hkl_phi(HklGeometry *g, HklVector *hkl)
{
      // do not forgot to update _hkl_phi
      hkl_smatrix R;

      _geometry.get_sample_rotation_matrix(&R);
      _geometry.get_Q(&_hkl_phi);

      ::hkl_smatrix_transpose(&R);
      ::hkl_smatrix_times_svector(&R, &_hkl_phi);
}
*/

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
	copy->reflections = hkl_list_new_copy(src->reflections);

	return copy;
}

void hkl_sample_free(HklSample *sample)
{
	hkl_lattice_free(sample->lattice);
	hkl_list_free(sample->reflections);
	free(sample);
}

void hkl_sample_get_UB(HklSample const *sample, HklMatrix *matrix)
{
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
*/

/*
void hkl_sample_fprintf(HklSample *sample, FILE *f)
{
	fprintf(f, "\"%s\"\n", sample->name);
}
*/

HklSampleReflection *hkl_sample_add_reflection(HklSample *sample,
		HklGeometry const *g, HklDetector const *det,
		double h, double k, double l)
{
	HklSampleReflection *ref;

	if (fabs(h) < HKL_EPSILON
			&& fabs(k) < HKL_EPSILON
			&& fabs(l) < HKL_EPSILON)
		return NULL;

	ref = malloc(sizeof(*ref));
	if (!ref)
		die("Cannot allocate memory for an HklSampleReflection");

	ref->geometry = hkl_geometry_new_copy(g);
	ref->detector = hkl_detector_new_copy(det);
	ref->hkl.data[0] = h;
	ref->hkl.data[1] = k;
	ref->hkl.data[2] = l;
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
