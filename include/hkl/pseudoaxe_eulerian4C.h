#ifndef _PSEUDOAXE_EULERIAN4C_H_
#define _PSEUDOAXE_EULERIAN4C_H_

#include "pseudoaxe.h"
#include "geometry_eulerian4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian4C {

            /*!
             * This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
             */
            class Vertical : public PseudoAxe
            {
            public:

              virtual ~Vertical(void); //!< The destructor

              virtual void initialize(Geometry const & geometry) = 0;

              virtual bool get_isValid(Geometry const & geometry) const = 0;

              virtual double const get_value(Geometry const & geometry) = 0;

              virtual void set_value(Geometry & geometry, double const & value) throw (HKLException) = 0;

              /*!
               * \brief Save the pseudoaxe::Eulerian4C into a stream.
               * \param flux the stream to save the pseudoaxe::Eulerian4C into.
               * \return The stream with the pseudoaxe::Eulerian4C.
               */
              ostream & toStream(ostream & flux) const;

              /*!
               * \brief Restore a pseudoaxe::Eulerian4C from a stream.
               * \param flux The stream containing the pseudoaxe::Eulerian4C.
               * \return The modified stream.
               */
              istream & fromStream(istream & flux);

            protected:
              geometry::eulerian4C::Vertical m_geometry_E4C; //!< The geometry use to initialize the pseudoaxe.

              Vertical(void); //!< Default constructor - protected to make sure this class is abstract.
            };

            namespace vertical {
                
                /*!
                 * The eulerian 4-circle Vertical diffractometer Psi pseudoAxe.
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
            class Psi : public Vertical
            {
            public:

              Psi(void); //!< Default constructor.

              virtual ~Psi(void); //!< Default destructor.

              void initialize(Geometry const & geometry);

              bool get_isValid(Geometry const & geometry) const;

              double const get_value(Geometry const & geometry);

              void set_value(Geometry & geometry, double const & value) throw (HKLException);

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
              svector m_Q; //!< The scattering vector Q.
            };

    } // namespace vertical
} // namespace eulerian4C
} // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
