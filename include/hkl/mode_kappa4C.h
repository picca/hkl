#ifndef _MODE_KAPPA4C_H_
#define _MODE_KAPPA4C_H_

#include "config.h"

#include "mode.h"
#include "mode_eulerian4C.h"
#include "geometry_kappa4C.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace mode {
        namespace kappa4C {
            /*!
             * This class defines the mode for all the 4 circles Kappa diffractometers.
             */
            class Vertical : public virtual Mode
            {
            public:

              virtual ~Vertical(void); //!< Default destructor

              virtual void computeAngles(double h, double k, double l,
                                         smatrix const & UB,
                                         Geometry & geometry) const = 0;

            protected:
              mutable geometry::eulerian4C::Vertical m_geometry_E4C; //!< The geometry::Eulerian4C use for the calculation

              Vertical(void); //!< Default constructor - protected to make sure this class is abstract.
            };

            namespace vertical {

#ifdef MSVC6
                class Bissector : public mode::kappa4C::Vertical
#else
                class Bissector : public mode::eulerian4C::vertical::Bissector, public mode::kappa4C::Vertical
#endif
                {
                public:

                  Bissector(void); //!< Default constructor.

                  virtual ~Bissector(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                protected:
                  mutable mode::eulerian4C::vertical::Bissector m_mode;
#endif
                };

#ifdef MSVC6
                class Delta_Theta : public mode::kappa4C::Vertical
#else
                class Delta_Theta : public mode::eulerian4C::vertical::Delta_Theta, public mode::kappa4C::Vertical
#endif
                {
                public:

                  Delta_Theta(void); //!< Default constructor.

                  virtual ~Delta_Theta(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                protected:
                  mutable mode::eulerian4C::vertical::Delta_Theta m_mode;
#endif
                };

#ifdef MSVC6
                class Constant_Omega : public mode::kappa4C::Vertical
#else
                class Constant_Omega : public mode::eulerian4C::vertical::Constant_Omega, public mode::kappa4C::Vertical
#endif
                {
                public:

                  Constant_Omega(void); //!< Default constructor.

                  virtual ~Constant_Omega(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                protected:
                  mutable mode::eulerian4C::vertical::Constant_Omega m_mode;
#endif
                };

#ifdef MSVC6
                class Constant_Chi : public mode::kappa4C::Vertical
#else
                class Constant_Chi : public mode::eulerian4C::vertical::Constant_Chi, public mode::kappa4C::Vertical
#endif
                {
                public:

                  Constant_Chi(void); //!< Default constructor.

                  virtual ~Constant_Chi(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                protected:
                  mutable mode::eulerian4C::vertical::Constant_Chi m_mode;
#endif
                };

#ifdef MSVC6
                class Constant_Phi : public mode::kappa4C::Vertical
#else
                class Constant_Phi : public mode::eulerian4C::vertical::Constant_Phi, public mode::kappa4C::Vertical
#endif
                {
                public:

                  Constant_Phi(void); //!< Default constructor.

                  virtual ~Constant_Phi(void); //!< Default destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);
#ifdef MSVC6
                protected:
                  mutable mode::eulerian4C::vertical::Constant_Phi m_mode;
#endif
                };

            } // namespace vertical
        } // namespace kappa4C
    } // namespace mode
} // namespace hkl

#endif // _KAPPA4C_H_
