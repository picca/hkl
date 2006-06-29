#ifndef _MODE_TWOC_H
#define _MODE_TWOC_H

#include "mode.h"
#include "geometry_twoC.h"

namespace hkl {
    namespace mode {
        namespace twoC {
            namespace vertical {

                /*!
                 * The Symetric mode of the vertical twoC Geometry.
                 *
                 * "omega" = \f$\theta\f$ and "2theta" = \f$2\theta\f$.
                 */
                class Symetric : public Mode<geometry::twoC::Vertical>
                {
                public:

                  Symetric(void); //!< Default constructor.

                  virtual ~Symetric(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::twoC::Vertical & geometry) const throw (HKLException);
                };

                /*!
                 * \brief The Delta Theta computational mode for the Eulerian4C diffractometer.
                 *
                 * "omega" = free, "2theta" = \f$2\theta\f$
                 */
                class Fix_Incidence : public Mode<geometry::twoC::Vertical>
                {
                public:

                  Fix_Incidence(void); //!< Default contructor.

                  virtual ~Fix_Incidence(void); //!< Default destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::twoC::Vertical & geometry) const throw (HKLException);

                };

            } // namespace vertical
        } // namespace twoC
    } // namespace mode
} // namespace hkl

#endif // _MODE_TWOC_H
