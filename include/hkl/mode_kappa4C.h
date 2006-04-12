#ifndef _MODE_KAPPA4C_H_
#define _MODE_KAPPA4C_H_

#include "mode.h"
#include "mode_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace mode {

        /*!
         * This class defines the mode for all the 4 circles Kappa diffractometers.
         */
        class Kappa4C : public virtual Mode
        {
        public:

          virtual ~Kappa4C(void); //!< Default destructor

          virtual void computeAngles(double h, double k, double l,
                                     smatrix const & UB,
                                     Geometry & geometry) const = 0;

        protected:
          mutable geometry::Eulerian4C m_geometry_E4C; //!< The geometry::Eulerian4C use for the calculation
          
          Kappa4C(void); //!< Default constructor - protected to make sure this class is abstract.
        };

        namespace kappa4C {

            class Bissector : public mode::eulerian4C::Bissector, public mode::Kappa4C
            {
            public:

              Bissector(void); //!< Default constructor.

              virtual ~Bissector(void); //!< Default Destructor.

              void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException);
            };

            class Delta_Theta : public mode::eulerian4C::Delta_Theta, public mode::Kappa4C
            {
            public:

              Delta_Theta(void); //!< Default constructor.

              virtual ~Delta_Theta(void); //!< Default Destructor.

              void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException);
            };

            class Constant_Omega : public mode::eulerian4C::Constant_Omega, public mode::Kappa4C
            {
            public:

              Constant_Omega(void); //!< Default constructor.

              virtual ~Constant_Omega(void); //!< Default Destructor.

              void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException);
            };

            class Constant_Chi : public mode::eulerian4C::Constant_Chi, public mode::Kappa4C
            {
            public:

              Constant_Chi(void); //!< Default constructor.

              virtual ~Constant_Chi(void); //!< Default Destructor.

              void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException);
            };

            class Constant_Phi : public mode::eulerian4C::Constant_Phi, public mode::Kappa4C
            {
            public:

              Constant_Phi(void); //!< Default constructor.

              virtual ~Constant_Phi(void); //!< Default destructor.

              void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException);
            };

        } // namespace kappa4C
    } // namespace mode
} // namespace hkl

#endif // _MODE_KAPPA4C_H_
