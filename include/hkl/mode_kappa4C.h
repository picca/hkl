#ifndef _MODE_KAPPA4C_H_
#define _MODE_KAPPA4C_H_

#include "derivedmode.h"
#include "mode_eulerian4C.h"
#include "geometry_kappa4C.h"

namespace hkl
  {
  namespace mode
    {
    namespace kappa4C
      {
      namespace vertical
        {
        namespace eulerian4C
          {

          typedef DerivedMode<mode::eulerian4C::vertical::Bissector, geometry::kappa4C::Vertical> Bissector; //!< DerivedMode from eulerian4C modes.
          typedef DerivedMode<mode::eulerian4C::vertical::Delta_Theta, geometry::kappa4C::Vertical> Delta_Theta; //!< DerivedMode from eulerian4C modes.
          typedef DerivedMode<mode::eulerian4C::vertical::Constant_Omega, geometry::kappa4C::Vertical> Constant_Omega; //!< DerivedMode from eulerian4C modes.
          typedef DerivedMode<mode::eulerian4C::vertical::Constant_Chi, geometry::kappa4C::Vertical> Constant_Chi; //!< DerivedMode from eulerian4C modes.
          typedef DerivedMode<mode::eulerian4C::vertical::Constant_Phi, geometry::kappa4C::Vertical> Constant_Phi; //!< DerivedMode from eulerian4C modes.

        } // namespace eulerian4C
      } // namespace vertical
    } // namespace kappa4C
  } // namespace mode
} // namespace hkl

#endif // _KAPPA4C_H_
