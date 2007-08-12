#ifndef _EULERIAN6C_MODE_H
#define _EULERIAN6C_MODE_H


#include "derived_mode.h"
#include "eulerian6C_geometry.h"
#include "eulerian4C_vertical_mode.h"

namespace hkl {

namespace eulerian6C {

namespace mode {

typedef hkl::mode::Derived<hkl::eulerian6C::Geometry, hkl::eulerian4C::vertical::mode::Bissector> Bissector;
typedef hkl::mode::Derived<hkl::eulerian6C::Geometry, hkl::eulerian4C::vertical::mode::Delta_Theta> Delta_Theta;
typedef hkl::mode::Derived<hkl::eulerian6C::Geometry, hkl::eulerian4C::vertical::mode::Constant_Omega> Constant_Omega;
typedef hkl::mode::Derived<hkl::eulerian6C::Geometry, hkl::eulerian4C::vertical::mode::Constant_Chi> Constant_Chi;
typedef hkl::mode::Derived<hkl::eulerian6C::Geometry, hkl::eulerian4C::vertical::mode::Constant_Phi> Constant_Phi;

} // namespace hkl::eulerian6C::mode

} // namespace hkl::eulerian6C

} // namespace hkl
#endif
