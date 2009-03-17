#include <hkl/hkl-pseudoaxis.h>

int hkl_pseudo_axis_engine_init_func(HklPseudoAxisEngine *self,
				     HklGeometry *geometry,
				     HklDetector const *detector,
				     HklSample const *sample)
{
	HklPseudoAxisEngineMode *getset;

	if (!self || !self->getset || !geometry || !detector || !sample)
		return HKL_FAIL;

	getset = self->getset;

	// update the geometry internals
	hkl_geometry_update(geometry);

	if(getset->geometry_init)
		hkl_geometry_free(getset->geometry_init);
	getset->geometry_init = hkl_geometry_new_copy(geometry);
	
	getset->detector_init = *detector;

	if(getset->sample_init)
		hkl_sample_free(getset->sample_init);
	getset->sample_init = hkl_sample_new_copy(sample);

	return HKL_SUCCESS;
}
