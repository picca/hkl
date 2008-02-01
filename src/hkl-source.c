#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include <hkl/hkl-source.h>

/*
   inline void source_compute_qi(struct hkl_quaternion * qi, double waveLength, HklVector const * direction)
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
	HklSource *source = NULL;
	source = malloc(sizeof(*source));
	if(!source)
		die("Cannot reserve memory for an Hklsource struct !!!");
	hkl_source_init(source, waveLength, x, y, z);
	return source;
}

void hkl_source_init(HklSource *source, double wave_length, double x, double y, double z)
{
	double norm;
	HklVector direction;

	direction.data[0] = x;
	direction.data[1] = y;
	direction.data[2] = z;
	norm = hkl_vector_norm2( &direction );
	if (wave_length > HKL_EPSILON && norm > HKL_EPSILON) {
		source->wave_length = wave_length;
		source->direction = direction;
		hkl_vector_div_double(&source->direction, norm);
	} else
		die("Can not initialize this source with thoses parameters wave length : %f, direction<%f, %f, %f>", wave_length, x, y, z);
}

void hkl_source_free(HklSource *source)
{
	free(source);
}

/** compare two sources */
int hkl_source_cmp(HklSource const *s1, HklSource const *s2)
{
	return ( (fabs(s1->wave_length - s2->wave_length) < HKL_EPSILON)
			&& hkl_vector_is_colinear(&s1->direction, &s2->direction));
}

/** compute the ki hkl_vector */
void hkl_source_get_ki(HklSource const *source, HklVector *ki)
{
	double k;

	k = HKL_TAU / source->wave_length;
	*ki = source->direction;
	hkl_vector_times_double( ki, k );
}
