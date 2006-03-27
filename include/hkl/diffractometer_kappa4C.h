#ifndef _DIFFRACTOMETER_KAPPA4C_H_
#define _DIFFRACTOMETER_KAPPA4C_H_

#include "diffractometer_kappa.h"

namespace hkl {
    namespace diffractometer {

        /**
         *  @brief This class describes a four-circle Kappa diffractometer.
         * 
         * The 4C Kappa diffractometer can be seen as a 4C eulerian one provided that we use some formula from the
         * MHATT-CAT, Advanced Photon Source, Argonne National Laboratory (
         * <A HREF="http://www.mhatt.aps.anl.gov/~walko/kappa.pdf">MHATT-CATs Newport Kappa Diffractometer</A>
         * written by Donald A. Walko). Other interesting documentation can be found at the 
         * <A HREF="http://www.px.nsls.bnl.gov/kappa.html">Brookhaven National Laboratory</A>
         */
        class Kappa4C : public Kappa
        {

        public:

          /**
           * @brief Default constructor
           */
          Kappa4C(double alpha);

          /**
           * @brief Destructor
           *
           * Destructor
           */
          virtual ~Kappa4C(void);
        };

    } // namespace diffractometer
} // namespace hkl

#endif // _DIFFRACTOMETER_KAPPA4C_H_
