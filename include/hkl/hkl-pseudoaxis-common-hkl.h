#include <hkl/hkl-pseudoaxis.h>

extern int RUBh_minus_Q(double const x[], void *params, double f[]);
extern int double_diffraction(double const x[], void *params, double f[]);

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
extern int hkl_pseudo_axis_engine_mode_get_hkl_real(HklPseudoAxisEngine *self,
						       HklGeometry *geometry,
						       HklDetector const *detector,
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
 * pseudo axis. You can use it if there is exactly 4 unknowns.
 * exemple : All E4CV constant axis mode or E6C vertical constant axis mode.
 */
extern int hkl_pseudo_axis_engine_mode_set_hkl_real(HklPseudoAxisEngine *self,
						       HklGeometry *geometry,
						       HklDetector *detector,
						       HklSample *sample);

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
extern int hkl_pseudo_axis_engine_mode_set_double_diffraction_real(HklPseudoAxisEngine *self,
								      HklGeometry *geometry,
								      HklDetector *detector,
								      HklSample *sample);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_hkl_new(void);
