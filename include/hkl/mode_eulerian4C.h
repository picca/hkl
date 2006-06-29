#ifndef _MODE_EULERIAN4C_H_
#define _MODE_EULERIAN4C_H_

#include "derivedmode.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace mode {
        namespace eulerian4C {
            namespace vertical {

                /*!
                 * The eulerian 4-circle diffractometer in bissector mode.
                 */
                class Bissector : public Mode<geometry::eulerian4C::Vertical>
                {
                public:

                  Bissector(void); //!< Default constructor.

                  virtual ~Bissector(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::eulerian4C::Vertical & geometry) const throw (HKLException);
                };

                /*!
                 * \brief The Delta Theta computational mode for the Eulerian4C diffractometer.
                 *
                 * In this mode \f$ \omega = \theta + d\theta \f$
                 */
                class Delta_Theta : public Mode<geometry::eulerian4C::Vertical>
                {
                public:

                  Delta_Theta(void); //!< Default contructor.

                  virtual ~Delta_Theta(void); //!< Default destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::eulerian4C::Vertical & geometry) const throw (HKLException);

                };

                /*!
                 * The eulerian 4-circle diffractometer in constant omega mode.
                 *
                 * The omega Axe is constant for this calculation.
                 */
                class Constant_Omega : public Mode<geometry::eulerian4C::Vertical>
                {
                public:

                  Constant_Omega(void); //!<! Default constructor.

                  virtual ~Constant_Omega(void); //!< Default destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::eulerian4C::Vertical & geometry) const throw (HKLException);
                };

                /*!
                 * The eulerian 4-circle diffractometer in constant chi mode.
                 *
                 * The chi Axe is constant for this calculation.
                 */
                class Constant_Chi : public Mode<geometry::eulerian4C::Vertical>
                {
                public:

                  Constant_Chi(void); //!< default constructor.

                  virtual ~Constant_Chi(void); //!< Default destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::eulerian4C::Vertical & geometry) const throw (HKLException);
                };

                /*!
                 * The eulerian 4-circle diffractometer in constant phi mode.
                 *
                 * The phi Axe is constant for this calculation.
                 */
                class Constant_Phi : public Mode<geometry::eulerian4C::Vertical>
                {
                public:

                  Constant_Phi(void); //!< The default constructor.

                  virtual ~Constant_Phi(void); //!< The destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::eulerian4C::Vertical & geometry) const throw (HKLException);
                };

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace mode
} // namespace hkl

#endif // _MODE_EULERIAN4C_H_
