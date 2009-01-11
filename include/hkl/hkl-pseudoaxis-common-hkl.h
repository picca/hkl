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
						  HklGeometry *geometry,
						  HklDetector const *detector,
						  HklSample const *sample);

/** 
 * @brief Standard setter method for the hkl double diffraction pseudoAxis
 * 
 * @param self 
 * @param geometry 
 * @param detector 
 * @param sample 
 * 
 * @return The status of the setter method.
 *
 * This method use only the hkl part of the equation to solve the 
 * pseudo axis. You can use it if there is exactly 4 unknowns.
 * exemple : All E4CV constant axis mode or E6C vertical constant axis mode.
 */
extern int hkl_pseudo_axis_engine_setter_func_hkl(HklPseudoAxisEngine *self,
						  HklGeometry *geometry,
						  HklDetector *detector,
						  HklSample *sample);

/**
 * @brief Create an HklPseudoAxisEngineGetSet for the double diffraction.
 *
 * @param name the name of the HklPseudoAxisEngineGetSet.
 * @param axes_names_len the length of the axes_names parameter
 * @param axes_names a vector with the names of the axes to use.
 */
extern HklPseudoAxisEngineGetSet *hkl_pseudo_axis_engine_get_set_double_diffraction_new(
	char const *name,
	size_t axes_names_len,
	char const *axes_names[]);
