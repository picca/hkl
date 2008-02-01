#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-source.h>

/*
   inline void s_compute_qi(struct hkl_quaternion * qi, double waveLength, HklVector const * direction)
   {
   double k;
   k = HKL_TAU / waveLength;

   qi->data[0] = 0.;
   qi->data[1] = direction->data[0] * k;
   qi->data[2] = direction->data[1] * k;
   qi->data[3] = direction->data[2] * k;
   }
   */

HklSource* hkl_source_new(double waveLength, double x, double y, double z)
{
	HklSource *s = NULL;

	s = malloc(sizeof(*s));
	if(!s)
		die("Cannot reserve memory for an Hkls struct !!!");

	s->direction = hkl_vector_new(0, 0, 0);
	hkl_source_set(s, waveLength, x, y, z);

	return s;
}

HklSource* hkl_source_new_copy(HklSource const *s)
{
	HklSource *copy = NULL;

	copy = malloc(sizeof(*copy));
	if(!copy)
		die("Cannot reserve memory for an Hkls struct !!!");

	copy->wave_length = s->wave_length;
	copy->direction = hkl_vector_new_copy(s->direction);

	return copy;
}

void hkl_source_set(HklSource *s,
		double wave_length, double x, double y, double z)
{
	if (wave_length > HKL_EPSILON && 
			( x > HKL_EPSILON
			  || y > HKL_EPSILON
			  || z > HKL_EPSILON)) {
		double norm;

		norm = sqrt(x*x + y*y + z*z);

		s->wave_length = wave_length;
		hkl_vector_set(s->direction, x, y, z);
		hkl_vector_div_double(s->direction, norm);
	}
}

void hkl_source_free(HklSource *s)
{
	hkl_vector_free(s->direction);
	free(s);
}

/** compare two sources */
int hkl_source_cmp(HklSource const *s1, HklSource const *s2)
{
	return ( (fabs(s1->wave_length - s2->wave_length) < HKL_EPSILON)
			&& hkl_vector_is_colinear(s1->direction, s2->direction));
}

/** compute the ki hkl_vector */
void hkl_source_get_ki(HklSource const *s, HklVector *ki)
{
	double k;

	k = HKL_TAU / s->wave_length;
	*ki = *s->direction;
	hkl_vector_times_double( ki, k );
}
