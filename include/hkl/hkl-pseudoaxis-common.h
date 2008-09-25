#include <hkl/hkl-pseudoaxis.h>

extern int RUBh_minus_Q(double const x[], void *params, double f[]);

extern int hkl_pseudo_axis_engine_getter_func_hkl(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector const *detector,
		HklSample const *sample);

extern int hkl_pseudo_axis_engine_setter_func_hkl(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample);
