#ifndef _CONSTANTS_H_
#define _CONSTANTS_H_

namespace hkl {
  namespace constant{
    namespace math {
      static double const pi = 3.14159265358979323846; //!< the \f$ \pi \f$ value.
      static double const epsilon = 1.e-6; //!< An \f$ \epsilon \f$ use to compare two angles.
      static double const epsilon2 = 2.e-6; //!< An \f$ \epsilon \f$ use to compare two angles.
      static double const epsilon_0 = 1.e-6; //!< An \f$ \epsilon \f$ use to compare two angles.
      static double const epsilon_1 = 1.e-10; //!< An \f$ \epsilon \f$ use to compare two doubles.
      static double const tiny = 1e-7; //!< A tiny value.
      static double const degToRad = 0.01745329251994330; //!< A value use to convert angles from degrees to radians.
      static double const radToDeg = 57.2957795130823208; //!< A value use to convert angles from radians to degrees.
      static int const precision = 15; //!< The precision use for the persistance of floats.
      } // namespace math
    namespace physic {
      //static double const tau = 1.; //!< The \f$ \tau \f$ constant (1. or \f$ \pi \f$).
      static double const tau = 2 * 3.14159265358979323846; //!< The \f$ \tau \f$ constant (1. or \f$ \pi \f$).
    } // namespace physic
  } // namespace constant
} // namespace hkl
      
#endif //_CONSTANTS_H_
