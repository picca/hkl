#include <hkl/hkl-pseudoaxis.h>

extern int RUBh_minus_Q(double const x[], void *params, double f[]);

/** 
 * @brief Standard getter for the hkl pseudoAxis.
 * 
 * @param self 
 * @param geometry 
 * @param detector 
 * @param sample 
 * 
 * @return the status of the getter method.
 *
 * This method can be used with all geometries of diffractometers
 * in getter/setter.
 */
extern int hkl_pseudo_axis_engine_getter_func_hkl(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector const *detector,
		HklSample const *sample);

/** 
 * @brief Standard setter method for the hkl pseudoAxis
 * 
 * @param self 
 * @param geometry 
 * @param detector 
 * @param sample 
 * 
 * @return The status of the setter method.
 *
 * This method use only the hkl part of the equation to solve the 
 * pseudo axis. You can use it if there is exactly 3 unknowns.
 * exemple : All E4CV constant axis mode or E6C vertical constant axis mode.
 */
extern int hkl_pseudo_axis_engine_setter_func_hkl(HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample);
