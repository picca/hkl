#ifndef _PSEUDOAXEENGINE_KAPPA6C_H_
#define _PSEUDOAXEENGINE_KAPPA6C_H_

#include "derivedpseudoaxeengine.h"
#include "pseudoaxeengine_eulerian4C.h"
#include "pseudoaxeengine_kappa4C.h"
#include "pseudoaxeengine_eulerian6C.h"
#include "geometry_kappa6C.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace kappa6C
      {
      namespace eulerian4C
        {
        namespace vertical
          {

          typedef DerivedPseudoAxeEngine<pseudoAxeEngine::eulerian4C::vertical::Psi, geometry::Kappa6C> Psi; //!< DerivedPseudoAxe from eulerian4C pseudoAxe.

        } // namespace vertical
      } // namespace eulerian4C
      namespace kappa4C
        {
        namespace vertical
          {
          typedef DerivedPseudoAxeEngine<pseudoAxeEngine::kappa4C::vertical::Eulerians, geometry::Kappa6C> Eulerians;
        } // namespace vertical
      } // namespace kappa4C

      namespace eulerian6C
        {

        typedef DerivedPseudoAxeEngine<pseudoAxeEngine::eulerian6C::Tth, geometry::Kappa6C> Tth; //!< DerivedPseudoAxe from eulerian6C pseudoAxe.
        typedef DerivedPseudoAxeEngine<pseudoAxeEngine::eulerian6C::Q, geometry::Kappa6C> Q; //!< DerivedPseudoAxe from eulerian6C pseudoAxe.

      } // namespace eulerian6C
    } // namespace kappa6C
  } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXEENGINE_KAPPA6C_H_
