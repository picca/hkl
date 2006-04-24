#ifndef _GEOMETRY_KAPPA4C_H_
#define _GEOMETRY_KAPPA4C_H_

#include "HKLException.h"
#include "geometry_kappa.h"
#include "geometry_eulerian4C.h"

namespace hkl {
    namespace geometry {
        namespace kappa4C {
            /**
             * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
             */
            class Vertical : public Kappa
            {
            public:

              /**
               * \brief The default constructor
               * @param alpha The alpha angle of the Kappa geometry.
               */
              Vertical(double alpha);

              /** 
               * @brief Constructor initializing the Axes.
               * @param alpha The alpha angle of the Kappa geometry.
               * @param komega The value of the "komega" Axe.
               * @param kappa The value of the "kappa" Axe.
               * @param kphi The value of the "kphi" Axe.
               * @param two_theta The value of the "2theta" Axe.
               */
              Vertical(double alpha, double komega, double kappa, double kphi, double two_theta);

              /**
               * \brief The destructor
               */
              virtual ~Vertical(void);

              /** 
               * @brief Set the kappa4C::Vertical geometry from an eulerian4C::Vertical geometry
               * @param E4C The eulerian4C::Vertical Geometry.
               * @throw HKLException if "chi" > 2 * alpha(kappa). 
               */
              void setFromGeometry(eulerian4C::Vertical const & E4C) throw (HKLException);
            };

        } // namespace kappa4C
    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA4C_H_
