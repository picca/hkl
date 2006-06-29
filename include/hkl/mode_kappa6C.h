#ifndef _MODE_KAPPA6C_H_
#define _MODE_KAPPA6C_H_

#include "derivedmode.h"
#include "mode_eulerian4C.h"
#include "geometry_kappa6C.h"

namespace hkl {
    namespace mode {
        namespace kappa6C {
            namespace eulerian4C {
                namespace vertical {

                    typedef DerivedMode<mode::eulerian4C::vertical::Bissector, geometry::Kappa6C> Bissector;
                    typedef DerivedMode<mode::eulerian4C::vertical::Delta_Theta, geometry::Kappa6C> Delta_Theta;
                    typedef DerivedMode<mode::eulerian4C::vertical::Constant_Omega, geometry::Kappa6C> Constant_Omega;
                    typedef DerivedMode<mode::eulerian4C::vertical::Constant_Chi, geometry::Kappa6C> Constant_Chi;
                    typedef DerivedMode<mode::eulerian4C::vertical::Constant_Phi, geometry::Kappa6C> Constant_Phi;

                } // namespace vertical
            } // namespace eulerian4C
        } // namespace kappa6C
    } // namespace mode
} // namespace hkl

#endif // _MODE_KAPPA6C_H_
