#include <math.h>
#include <assert.h>

#include "config.h"
#include "source.h"

/*
   inline void source_compute_qi(struct hkl_quaternion * qi, double waveLength, struct hkl_svector const * direction)
   {
   double k;
   k = HKL_TAU / waveLength;

   qi->data[0] = 0.;
   qi->data[1] = direction->data[0] * k;
   qi->data[2] = direction->data[1] * k;
   qi->data[3] = direction->data[2] * k;
   }
   */

void hkl_source_init(struct hkl_source *source, double wave_length, double x, double y, double z)
{
	double norm;
	struct hkl_svector direction;

	direction.data[0] = x;
	direction.data[1] = y;
	direction.data[2] = z;
	norm = hkl_svector_norm2( &direction );
	if (wave_length > HKL_EPSILON && norm > HKL_EPSILON) {
		source->wave_length = wave_length;
		source->direction = direction;
		hkl_svector_div_double(&source->direction, norm);
	} else
		die("Can not initialize this source with thoses parameters wave length : %f, direction<%f, %f, %f>", wave_length, x, y, z);
}

/** compare two sources */
int hkl_source_cmp(struct hkl_source const * s1, struct hkl_source const * s2)
{
	return ( (fabs(s1->wave_length - s2->wave_length) < HKL_EPSILON)
			&& hkl_svector_is_colinear(&s1->direction, &s2->direction));
}

/** compute the ki hkl_svector */
void hkl_source_get_ki(struct hkl_source const * source, struct hkl_svector * ki)
{
	double k;

	k = HKL_TAU / source->wave_length;
	*ki = source->direction;
	hkl_svector_times_double( ki, k );
}
