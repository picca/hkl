#ifndef _PSEUDOAXE_KAPPA6C_H_
#define _PSEUDOAXE_KAPPA6C_H_

#include "derivedpseudoaxe.h"
#include "pseudoaxe_kappa4C.h"
#include "pseudoaxe_eulerian4C.h"
#include "pseudoaxe_eulerian6C.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace kappa6C
      {
      namespace eulerian4C
        {
        namespace vertical
          {

          typedef DerivedPseudoAxe<pseudoAxe::eulerian4C::vertical::Psi, geometry::Kappa6C> Psi; //!< DerivedPseudoAxe from eulerian4C pseudoAxe.

        } // namespace vertical
      } // namespace eulerian4C
      namespace kappa4C
        {
        namespace vertical
          {

          typedef DerivedPseudoAxe<pseudoAxe::kappa4C::vertical::Omega, geometry::Kappa6C> Omega; //!< DerivedPseudoAxe from kappa4C pseudoAxe.
          typedef DerivedPseudoAxe<pseudoAxe::kappa4C::vertical::Chi, geometry::Kappa6C> Chi; //!< DerivedPseudoAxe from kappa4C pseudoAxe.
          typedef DerivedPseudoAxe<pseudoAxe::kappa4C::vertical::Phi, geometry::Kappa6C> Phi; //!< DerivedPseudoAxe from kappa4C pseudoAxe.

        } // namespace vertical
      } // namespace kappa4C

      namespace eulerian6C
        {

        typedef DerivedPseudoAxe<pseudoAxe::eulerian6C::Tth, geometry::Kappa6C> Tth; //!< DerivedPseudoAxe from eulerian6C pseudoAxe.
        typedef DerivedPseudoAxe<pseudoAxe::eulerian6C::Q, geometry::Kappa6C> Q; //!< DerivedPseudoAxe from eulerian6C pseudoAxe.

      } // namespace eulerian6C
    } // namespace kappa6C
  } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
