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
  double norm;

  // check that the source parameters is ok.
  assert( source->wave_length > HKL_EPSILON);
  norm = hkl_svector_norm2( &source->direction );
  assert( norm > HKL_EPSILON );

  *ki = source->direction;
  hkl_svector_div_double( ki, norm );

  // compute the ki norm
  k = HKL_TAU / source->wave_length;
  hkl_svector_times_double( ki, k );
}
