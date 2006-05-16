#ifndef _MODE_TWOC_H
#define _MODE_TWOC_H

#include "mode.h"

namespace hkl {
    namespace mode {
        namespace twoC {
            namespace vertical {

                /*!
                 * The Symetric mode of the vertical twoC Geometry.
                 *
                 * "omega" = \f$\theta\f$ and "2theta" = \f$2\theta\f$.
                 */
                class Symetric : public Mode
                {
                public:

                  Symetric(void); //!< Default constructor.

                  virtual ~Symetric(void); //!< Default Destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);
                };

                /*!
                 * \brief The Delta Theta computational mode for the Eulerian4C diffractometer.
                 *
                 * "omega" = free, "2theta" = \f$2\theta\f$
                 */
                class Fix_Incidence : public Mode
                {
                public:

                  Fix_Incidence(void); //!< Default contructor.

                  virtual ~Fix_Incidence(void); //!< Default destructor.

                  virtual void computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException);

                };

            } // namespace vertical
        } // namespace twoC
    } // namespace mode
} // namespace hkl

#endif // _MODE_TWOC_H
