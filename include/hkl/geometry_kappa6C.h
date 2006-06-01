#ifndef _GEOMETRY_KAPPA6C_H_
#define _GEOMETRY_KAPPA6C_H_

#include "HKLException.h"
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

          /** 
           * @brief Set the angles of the eulerian6C geometry.
           * 
           * @param mu The value of the "mu" Axe.
           * @param omega The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param gamma The value of the "gamma" Axe.
           * @param delta The value of the "delta" Axe.
           */
          void setAngles(double const & mu, double const & komega, double const & kappa, double const & kphi,
                         double const & gamma, double const & delta);

          /** 
           * @brief Set a kappa6C Geometry from an other Geometry.
           * 
           * @param geometry The Geometry
           * @param strict
           * @throw HKLException if "chi" > 2 * alpha(kappa)
           */
          void setFromGeometry(Geometry const & geometry, bool const & strict) throw (HKLException);
        };

    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA6C_H_
