#include <math.h>
#include <hkl/hkl-detector.h>

/* public part */

int hkl_detector_compute_kf(HklDetector const *self, HklGeometry *g,
		HklVector *kf)
{
	hkl_geometry_update(g);

	HklHolder *holder = &g->holders[self->idx];
	if (holder) {
		hkl_vector_init(kf, HKL_TAU / g->source.wave_length, 0, 0);
		hkl_vector_rotated_quaternion(kf, &holder->q);
		return HKL_SUCCESS;
	} else
		return HKL_FAIL;
}
