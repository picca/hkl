#ifndef _KAPPA6C_MODE_H
#define _KAPPA6C_MODE_H


#include "derived_mode.h"
#include "kappa6C_geometry.h"
#include "eulerian4C_vertical_mode.h"

namespace hkl {

namespace kappa6C {

namespace mode {

typedef hkl::mode::Derived<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::mode::Bissector> Bissector;
typedef hkl::mode::Derived<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::mode::Constant_Omega> Constant_Omega;
typedef hkl::mode::Derived<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::mode::Constant_Chi> Constant_Chi;
typedef hkl::mode::Derived<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::mode::Constant_Phi> Constant_Phi;
typedef hkl::mode::Derived<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::mode::Delta_Theta> Delta_Theta;

} // namespace hkl::kappa6C::mode

} // namespace hkl::kappa6C

} // namespace hkl
#endif
