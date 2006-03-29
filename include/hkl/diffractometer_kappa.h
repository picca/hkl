#ifndef _DIFFRACTOMETER_KAPPA_H_
#define _DIFFRACTOMETER_KAPPA_H_

#include "diffractometer.h"
#include "geometry_kappa.h"

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
        class Kappa : public Diffractometer
        {

        public:

          /**
           * @brief Destructor
           *
           * Destructor
           */
          virtual ~Kappa(void);

        protected:
          /**
           * @brief Default constructor - Protected
           */
          Kappa(double alpha);

        };

    } // namespace diffractometer
} // namespace hkl

#endif // _DIFFRACTOMETER_KAPPA_H_
