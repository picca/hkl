#ifndef _PSEUDOAXE_EULERIAN4C_H_
#define _PSEUDOAXE_EULERIAN4C_H_

#include "pseudoaxe.h"
#include "geometry_eulerian4C.h"

using namespace std;

namespace hkl {
  namespace pseudoAxe {

    /**
     * This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
     */
    class Eulerian4C : public PseudoAxe
    {
      public:

        virtual ~Eulerian4C(void); //<! The destructor

        /**
         * \brief Initialize the PseudoAxe from the Geometry.
         * \param geometry The configuration to save for calculation.
         */
        virtual void init(Geometry const & geometry) = 0;
          
        /**
         * \brief get the current value of the PseudoAxe.
         * \param geometry the Geometry containing the real #Axe
         * \return the position of the PseudoAxe.
         */
        virtual double const get_value(Geometry const & geometry) const = 0;

        /**
         * \brief set the current value of the PseudoAxe.
         * \param geometry the Geometry containing the real #Axe
         * \param value the value to set.
         */
        virtual void set_value(Geometry & geometry, double value) throw (HKLException) = 0;

      protected:
        geometry::Eulerian4C m_geometry_E4C; //The geometry use to initialize the pseudoaxe.
 
        Eulerian4C(void); //<! Default constructor - protected to make sure this class is abstract.
    };

    namespace eulerian4C {
      
      /**
       * The eulerian 4-circle diffractometer Psi pseudoAxe.
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
       * Now \f$\psi\f$ is the rotation angle around the scattering vector \f$ \vec{Q} \f$.
       * So we can write:
       * \f[
       *  R = \Psi R_0
       * \f]
       * where \f$\Psi\f$ is the rotation matrix around the \f$ \vec{Q} \f$ vector.
       *
       */
      class Psi : public Eulerian4C
      {
        public:

          Psi(void); //<! The default constructor.

          Psi(Psi const & psi); //<! Copy constructor.
        
          virtual ~Psi(void); //<! The Destructor.

          /**
           * \brief Initialize the PseudoAxe from the Geometry.
           * \param geometry The configuration to save for calculation.
           */
          void init(Geometry const & geometry);
          
          /**
           * \brief get the current value of the PseudoAxe.
           * \param geometry the Geometry containing the real #Axe
           * \return the position of the PseudoAxe.
           */
          double const get_value(Geometry const & geometry) const;

          /**
           * \brief set the current value of the PseudoAxe.
           * \param geometry the Geometry containing the real #Axe
           * \param value the value to set.
           */
          void set_value(Geometry & geometry, double value) throw (HKLException);
        
        private:
          svector m_Q; //<! The scattering vector Q.
      };

    } // namespace eulerian4C
  } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
