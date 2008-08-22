#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-source.h>

int hkl_source_init(HklSource *self,
		double wave_length, double x, double y, double z)
{
	if (wave_length > HKL_EPSILON && 
			( x > HKL_EPSILON
			  || y > HKL_EPSILON
			  || z > HKL_EPSILON)) {
		double norm;

		norm = sqrt(x*x + y*y + z*z);

		self->wave_length = wave_length;
		hkl_vector_init(&self->direction, x, y, z);
		hkl_vector_div_double(&self->direction, norm);
		return HKL_SUCCESS;
	} else
		return HKL_FAIL;
}


/** compare two sources */
int hkl_source_cmp(HklSource const *self, HklSource const *s)
{
	return ( (fabs(self->wave_length - s->wave_length) < HKL_EPSILON)
			&& hkl_vector_is_colinear(&self->direction,
				&s->direction));
}

/** compute the ki hkl_vector */
void hkl_source_compute_ki(HklSource const *self, HklVector *ki)
{
	double k;

	k = HKL_TAU / self->wave_length;
	*ki = self->direction;
	hkl_vector_times_double(ki, k);
}
