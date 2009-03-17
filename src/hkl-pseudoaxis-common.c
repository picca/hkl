#include <hkl/hkl-pseudoaxis.h>

int hkl_pseudo_axis_engine_init_func(HklPseudoAxisEngine *self,
				     HklGeometry *geometry,
				     HklDetector const *detector,
				     HklSample const *sample)
{
	HklPseudoAxisEngineMode *mode;

	if (!self || !self->mode || !geometry || !detector || !sample)
		return HKL_FAIL;

	mode = self->mode;

	// update the geometry internals
	hkl_geometry_update(geometry);

	if(mode->geometry_init)
		hkl_geometry_free(mode->geometry_init);
	mode->geometry_init = hkl_geometry_new_copy(geometry);
	
	mode->detector_init = *detector;

	if(mode->sample_init)
		hkl_sample_free(mode->sample_init);
	mode->sample_init = hkl_sample_new_copy(sample);

	return HKL_SUCCESS;
}
