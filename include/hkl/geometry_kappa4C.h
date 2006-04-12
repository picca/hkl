#ifndef _GEOMETRY_KAPPA4C_H_
#define _GEOMETRY_KAPPA4C_H_

#include "geometry_kappa.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace geometry {

        class Eulerian4C;

        /**
         * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
         */
        class Kappa4C : public Kappa
        {
        public:

          /**
           * \brief The default constructor
           * @param alpha The alpha angle of the Kappa geometry.
           */
          Kappa4C(double alpha);

          /** 
           * @brief Constructor initializing the Axes.
           * @param alpha The alpha angle of the Kappa geometry.
           * @param komega The value of the "komega" Axe.
           * @param kappa The value of the "kappa" Axe.
           * @param kphi The value of the "kphi" Axe.
           * @param two_theta The value of the "2theta" Axe.
           */
          Kappa4C(double alpha, double komega, double kappa, double kphi, double two_theta);

          /**
           * \brief The destructor
           */
          virtual ~Kappa4C(void);

          void setFromE4C(Eulerian4C const & E4C);
        };

    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA4C_H_
