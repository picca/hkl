#include <math.h>
#include <hkl/hkl-detector.h>

/* public part */

HklDetector *hkl_detector_new(void)
{
	HklDetector *det = NULL;

	det = malloc(sizeof(*det));
	if (!det)
		die("Cannot allocate a HklDetector struct !!!");
	
	det->idx = 0;

	return det;
}

HklDetector *hkl_detector_new_copy(HklDetector const *src)
{
	HklDetector *copy = NULL;

	copy = hkl_detector_new();
	copy->idx = src->idx;

	return copy;
}

void hkl_detector_free(HklDetector *d)
{
	free(d);
}

int hkl_detector_get_kf(HklDetector const *det, HklGeometry *g, HklVector *kf)
{
	hkl_geometry_update(g);

	HklHolder *holder = &g->holders[det->idx];
	if (holder) {
		hkl_vector_init(kf, HKL_TAU / g->source.wave_length, 0, 0);
		hkl_vector_rotated_quaternion(kf, &holder->q);
		return HKL_SUCCESS;
	} else
		return HKL_FAIL;
}
