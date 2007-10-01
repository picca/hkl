#ifndef _EULERIAN6C_PSEUDOAXEENGINE_H
#define _EULERIAN6C_PSEUDOAXEENGINE_H


#include "pseudoaxeengine.h"
#include "svecmat.h"
#include "eulerian6C_geometry.h"
#include "HKLException.h"
#include <ostream>
#include <istream>
#include "interval.h"
#include "derived_pseudoaxeengine.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"

namespace hkl
  {
  class Parameter;
}
namespace hkl
  {
  namespace axe
    {
    class Rotation;
  }
}
namespace hkl
  {
  class PseudoAxe;
}

namespace hkl
  {

  namespace eulerian6C
    {

    namespace pseudoAxeEngine
      {

      /**
       * @brief The eulerian 6-circle Vertical diffractometer Tth pseudoAxe.
       *
       * This pseudoAxe keep the detector in the scattering plan define by \f$ \vec{k_i} \f$ and \f$ \vec{k_f} \f$.
       * Its value correspond to the angle between thoses two vectors.
       *
       * We start from :
       * \f[
       *   \vec{k_i} =
       *     \left(
       *       \begin{matrix}
       *         1 \\
       *         0 \\
       *         0
       *       \end{matrix}
       *     \right)
       *   \hspace{1cm}
       *   \vec{k_f} =
       *     \left(
       *       \begin{matrix}
       *         \cos\gamma \cos\delta \\
       *         \sin\gamma \cos\delta \\
       *         \sin\delta
       *       \end{matrix}
       *     \right)
       * \f]
       * to compute the \f$ 2\theta \f$ angle, we can use the scalar product.
       * Indeed :
       * \f[
       *   \vec{k_i} \cdot \vec{k_f} = k_i \cdot k_f \cdot \cos\left(\widehat{\vec{k_i},\vec{k_f}}\right)
       * \f]
       * so
       * \f[
       *   2\theta = \arccos ( \cos\gamma \cos\delta ) \hspace{1cm} \arccos x \in [0, \pi]
       * \f]
       * nevertheless we need to orientate the space to disambiguate this equation.
       * So we decide to give :
       * \f[
       *   \mbox{sign of}\ 2\theta = \mbox{sign of}\ \delta\ \mbox{when}\ \delta \neq 0
       * \f]
       * and
       * \f[
       *   \mbox{sign of}\ 2\theta = \mbox{sign of}\ \gamma\ \mbox{when}\ \delta = 0
       * \f]
       */

      class Tth : public hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry>
        {
        protected:
          hkl::Parameter * _direction;

          hkl::axe::Rotation * _gamma;

          double _gamma0;

          hkl::axe::Rotation * _delta;

          double _delta0;

          hkl_svector _axe0;

          hkl::PseudoAxe * _tth;


        public:
          Tth(hkl::eulerian6C::Geometry & geometry);

          ~Tth();

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

        protected:
          /**
           * @brief Compute the tth angle from gamma and delta
           * @param gamma The gamma angle.
           * @param delta The gamma angle.
           * @param writable Is the axe compatible with the initialization.
           * @return the tth angle.
           */
          double compute_tth(double gamma, double delta, bool & writable);

          /**
           * @brief compute the range of the tth pseudoAxe.
           * @param min the minimum value computed
           * @param max the maximum value computed
           */
          void compute_tth_range(double & min, double & max);

        };
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

      class Q : public hkl::PseudoAxeEngineTemp<hkl::eulerian6C::Geometry>
        {
        protected:
          hkl::axe::Rotation * _gamma;

          hkl::axe::Rotation * _delta;

          hkl::PseudoAxe * _q;

          hkl::eulerian6C::pseudoAxeEngine::Tth * _tth_engine;

          hkl::PseudoAxe * _tth;


        public:
          Q(hkl::eulerian6C::Geometry & geometry);

          ~Q();

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

        protected:
          /**
           * @brief compute the range of the Q pseudoAxe.
           * @param min the minimum value computed
           * @param max the maximum value computed
           */
          void compute_q_range(double & min, double & max);

        };
      typedef hkl::pseudoAxeEngine::DerivedWithSample<hkl::eulerian6C::Geometry, hkl::eulerian4C::vertical::pseudoAxeEngine::Psi> Psi;

    } // namespace hkl::eulerian6C::pseudoAxeEngine

  } // namespace hkl::eulerian6C

} // namespace hkl
#endif
