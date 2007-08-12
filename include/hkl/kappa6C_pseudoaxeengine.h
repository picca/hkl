#ifndef _KAPPA6C_PSEUDOAXEENGINE_H
#define _KAPPA6C_PSEUDOAXEENGINE_H


#include "derived_pseudoaxeengine.h"
#include "kappa6C_geometry.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "kappa4C_vertical_pseudoaxeengine.h"
#include "eulerian6C_pseudoaxeengine.h"

namespace hkl {

namespace kappa6C {

namespace pseudoAxeEngine {

typedef hkl::pseudoAxeEngine::DerivedWithSample<hkl::kappa6C::Geometry, hkl::eulerian4C::vertical::pseudoAxeEngine::Psi> Psi;
typedef hkl::pseudoAxeEngine::Derived<hkl::kappa6C::Geometry, hkl::kappa4C::vertical::pseudoAxeEngine::Eulerians> Eulerians;
typedef hkl::pseudoAxeEngine::Derived<hkl::kappa6C::Geometry, hkl::eulerian6C::pseudoAxeEngine::Tth> Tth;
typedef hkl::pseudoAxeEngine::Derived<hkl::kappa6C::Geometry, hkl::eulerian6C::pseudoAxeEngine::Q> Q;

} // namespace hkl::kappa6C::pseudoAxeEngine

} // namespace hkl::kappa6C

} // namespace hkl
#endif
