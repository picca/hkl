#ifndef _GEOMETRY_KAPPA6C_H_
#define _GEOMETRY_KAPPA6C_H_

#include "geometry_kappa.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace geometry {

        /**
         * \brief A Geometry for a the kappa 6 circle soleil generic diffractometer.
         */
        class Kappa6C : public Kappa
        {
        public:

          /**
           * \brief The default constructor
           *
           * @param alpha The alpha angle of the Kappa geometry.
           */
          Kappa6C(double alpha);

          /** 
           * @brief A constructor which set the axes.
           *
           * @param alpha The alpha angle of the Kappa geometry.
           * @param mu The value of the "mu" Axe
           * @param komega The value of the "komega" Axe
           * @param kappa The value of the "kappa" Axe
           * @param kphi The value of the "kphi" Axe
           * @param gamma The value of the "gamma" Axe
           * @param delta The value of the "delta" Axe
           */
          Kappa6C(double alpha, double mu, double komega, double kappa, double kphi, double gamma, double delta);

          /**
           * \brief The destructor
           */
          virtual ~Kappa6C(void);

          void setFromGeometry(eulerian4C::Vertical const & E4C);
        };

    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA6C_H_
