#ifndef _KAPPA4C_VERTICAL_MODE_H
#define _KAPPA4C_VERTICAL_MODE_H


#include "derived_mode.h"
#include "kappa4C_vertical_geometry.h"
#include "eulerian4C_vertical_mode.h"

namespace hkl {

namespace kappa4C {

namespace vertical {

namespace mode {

typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Bissector> Bissector;
typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Delta_Theta> Delta_Theta;
typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Constant_Omega> Constant_Omega;
typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Constant_Chi> Constant_Chi;
typedef hkl::mode::Derived<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::mode::Constant_Phi> Constant_Phi;

} // namespace hkl::kappa4C::vertical::mode

} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
#endif
