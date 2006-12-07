#ifndef _PSEUDOAXE_EULERIAN4C_H_
#define _PSEUDOAXE_EULERIAN4C_H_

#include "derivedpseudoaxe.h"
#include "geometry_eulerian4C.h"
#include "pseudoaxe_twoC.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace eulerian4C
      {
      namespace vertical
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
        class Psi : public PseudoAxeTemp<geometry::eulerian4C::Vertical>
          {
          public:

            Psi(geometry::eulerian4C::Vertical &); //!< Default constructor.

            void initialize(void) throw (HKLException);

            void uninitialize(void);

            bool isValid(void) throw (HKLException);

            void update(void);

            void set_current(Value const & value) throw (HKLException);

            /*!
             * \brief Save the pseudoaxe::eulerian4C::Psi into a stream.
             * \param flux the stream to save the pseudoaxe::eulerian4C::Psi into.
             * \return The stream with the pseudoaxe::eulerian4C.
             */
            ostream & toStream(ostream & flux) const;

            /*!
             * \brief Restore a pseudoaxe::eulerian4C::Psi from a stream.
             * \param flux The stream containing the pseudoaxe::eulerian4C::Psi.
             */
            istream & fromStream(istream & flux);


          private:
            Axe & _omega;
            Axe & _chi;
            Axe & _phi;
            Axe & _tth;

            svector _Q0; //!< The scattering vector Q.
            Quaternion _qpsi0;
          };

        namespace twoC
          {

          typedef DerivedPseudoAxe<pseudoAxe::twoC::vertical::Th2th, geometry::eulerian4C::Vertical> Th2th;
          typedef DerivedPseudoAxe<pseudoAxe::twoC::vertical::Q2th, geometry::eulerian4C::Vertical> Q2th;
          typedef DerivedPseudoAxe<pseudoAxe::twoC::vertical::Q, geometry::eulerian4C::Vertical> Q;

        } // namespace twoC
      } // namespace vertical.
    } // namespace eulerian4C.
  } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_EULERIAN4C_H_
