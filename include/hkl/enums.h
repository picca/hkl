#ifndef _ENUMS_H_
#define _ENUMS_H_

namespace hkl
  {

  /**
   * An enum use to define the sample type store in a SampleList.
   */
  enum SampleType
  {
    SAMPLE_MONOCRYSTAL
  };

  /**
   * An enum use to define the reflection type store in a ReflectionList.
   */
  enum ReflectionType
  {
    REFLECTION_MONOCRYSTAL
  };

  /**
   * An enum use to define the diffractometer type.
   */
  enum DiffractometerType
  {
    DIFFRACTOMETER_TWOC_VERTICAL,
    DIFFRACTOMETER_EULERIAN4C_VERTICAL,
    DIFFRACTOMETER_EULERIAN6C,
    DIFFRACTOMETER_KAPPA4C_VERTICAL,
    DIFFRACTOMETER_KAPPA6C
  };
} // namespace hkl

#endif // _ENUMS_H_
