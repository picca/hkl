/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _KAPPA4C_VERTICAL_PSEUDOAXEENGINE_H
#define _KAPPA4C_VERTICAL_PSEUDOAXEENGINE_H


#include "pseudoaxeengine.h"
#include "kappa4C_vertical_geometry.h"
#include "HKLException.h"
#include <ostream>
#include <istream>
#include "derived_pseudoaxeengine.h"
#include "twoC_vertical_pseudoaxeengine.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"

namespace hkl
  {
  namespace axe
    {
    class Rotation;
  }
}
namespace hkl
  {
  class Parameter;
}
namespace hkl
  {
  class PseudoAxe;
}

namespace hkl
  {

  namespace kappa4C
    {

    namespace vertical
      {

      namespace pseudoAxeEngine
        {

        /**
         * @brief The eulerian 4-circle Vertical diffractometer Psi pseudoAxe.
         *
         * This pseudoAxe represent a rotation around the scattering vector \f$ \vec{Q} \f$.
         *
         * To compute this pseudoAxe we start from the fondamental equation
         * \f[
         *  R = \Omega \chi \phi
         * \f]
         * thus
         * \f[
         *  \left(
         *    \begin{matrix}
         *      \cos\omega \cos\phi - \cos\chi \sin\omega \sin\phi & -\sin\chi \sin\omega & -\cos\omega \sin\phi - \cos\chi \sin\omega \cos\phi \\
         *      -\sin\chi \sin\phi                                 & \cos\chi             & -\sin\chi \cos\phi \\
         *      \sin\omega \cos\phi - \cos\chi \cos\omega \sin\phi & \sin\chi \cos\omega  & -\sin\omega \sin\phi + \cos\chi \cos\omega \cos\phi
         *    \end{matrix}
         *  \right)
         * \f]
         *
         * Now \f$\psi\f$ is the rotation angle around the scattering vector \f$\vec{Q}\f$.
         * So we can write:
         * \f[
         *  R = \Psi R_0
         * \f]
         * where \f$\Psi\f$ is the rotation matrix around the \f$\vec{Q}\f$ vector
         * and \f$R_0\f$ the \f$R\f$ matrix for \f$\Psi=0\f$
         *
         * The 1st where \f$\sin\chi > 0\f$ is:
         * \f{eqnarray*}
         *  \omega & = & \arctan(-R_{0,1}, R_{2,1}) \\
         *  \chi & = & \arctan(\sqrt{R_{0,1}^2+R_{2,1}^2}, R_{1,1}) \\
         *  \phi & = & \arctan(-R_{1,0}, -R_{1,2})
         * \f}
         *
         * The 2nd one \f$\sin\chi < 0\f$ is:
         * \f{eqnarray*}
         *  \omega' & = & \arctan(R_{0,1}, -R_{2,1}) \\
         *  \chi' & = & \arctan(-\sqrt{R_{0,1}^2+R_{2,1}^2}, R_{1,1}) = -\chi\\
         *  \phi' & = & \arctan(R_{1,0}, R_{1,2})
         * \f}
         *
         * Thoses two solutions are not valid if \f$\chi=0\f$ or \f$\chi=\pi\f$.
         * In that case the \f$ R\f$ matrix can be simplify:
         * \f[
         *  \left(
         *    \begin{matrix}
         *      \pm\cos(\omega+\phi) & 0 & \mp\sin(\omega+\phi) \\
         *      0                 & \pm1 & 0 \\
         *      \pm\sin(\omega+\phi) & 0 & \pm\cos(\omega+\phi)
         *    \end{matrix}
         *  \right)
         * \f]
         */

        class Eulerians : public hkl::PseudoAxeEngineTemp<hkl::kappa4C::vertical::Geometry>
          {
          protected:
            double _alpha;

            hkl::axe::Rotation * _komega;

            hkl::axe::Rotation * _kappa;

            hkl::axe::Rotation * _kphi;

            hkl::Parameter * _solution;

            hkl::PseudoAxe * _omega;

            hkl::PseudoAxe * _chi;

            hkl::PseudoAxe * _phi;


          public:
            Eulerians(hkl::kappa4C::vertical::Geometry & geometry);

            ~Eulerians();

            /**
             * @brief Initialize the pseudoAxe.
             *
             * This method must be call before using a pseudoAxe.
             */
            virtual void initialize() throw(hkl::HKLException);

            virtual void update();

            /**
             * @brief set the current value of the PseudoAxe.
             * @throw HKLException if the pseudoAxe is not ready to be set.
             */
            virtual void set() throw(hkl::HKLException);

            /**
             * @brief print on a stream the content of the Eulerians
             * @param flux the ostream to modify.
             * @return the modified ostream
             */
            std::ostream & toStream(std::ostream & flux) const;

            /**
             * @brief restore the content of the Eulerians from an istream
             * @param flux the istream.
             * @return the modified istream.
             * @todo problem of security here.
             */
            std::istream & fromStream(std::istream & flux);

          };
        typedef hkl::pseudoAxeEngine::Derived<hkl::kappa4C::vertical::Geometry, hkl::twoC::vertical::pseudoAxeEngine::Th2th> Th2th;
        typedef hkl::pseudoAxeEngine::Derived<hkl::kappa4C::vertical::Geometry, hkl::twoC::vertical::pseudoAxeEngine::Q2th> Q2th;
        typedef hkl::pseudoAxeEngine::Derived<hkl::kappa4C::vertical::Geometry, hkl::twoC::vertical::pseudoAxeEngine::Q> Q;
        typedef hkl::pseudoAxeEngine::DerivedWithSample<hkl::kappa4C::vertical::Geometry, hkl::eulerian4C::vertical::pseudoAxeEngine::Psi> Psi;

      } // namespace hkl::kappa4C::vertical::pseudoAxeEngine

    } // namespace hkl::kappa4C::vertical

  } // namespace hkl::kappa4C

} // namespace hkl
#endif
