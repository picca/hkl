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

void hkl_detector_free(HklDetector *d)
{
	free(d);
}

int hkl_detector_get_kf(HklDetector const *det, HklGeometry *g, 
		HklVector *kf, HklVector *kfc)
{
	hkl_geometry_update(g);

	HklHolder *holder = hkl_geometry_get_holder(g, det->idx);
	if (holder) {
		hkl_vector_set(kf, HKL_TAU / g->source->wave_length, 0, 0);
		hkl_vector_set(kfc, HKL_TAU / g->source->wave_length, 0, 0);
		hkl_holder_apply_to_vector(holder, kf, kfc);
		return HKL_SUCCESS;
	} else
		return HKL_FAIL;
}
