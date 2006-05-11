#ifndef _GEOMETRY_TWOC_H_
#define _GEOMETRY_TWOC_H_

#include "geometry.h"

using namespace std;

namespace hkl {
    namespace geometry {

        //forward declaration
        namespace eulerian4C {
            class Vertical;
            class Horizontal;
        }
        namespace kappa4C {
            class Vertical;
            class Horizontal;
        }
        class Kappa6C;

        namespace twoC {

            /**
             * \brief A Geometry for the Vertical eulerian 4 circle soleil generic diffractometer.
             */
            class Vertical : public Geometry
            { 
            public:

              Vertical(void); //!< Default constructor.

              Vertical(Vertical const & geometry); //!< Copy Constructor.

              /** 
               * @brief Constructor
               * 
               * @param omega The value of the "omega" Axe.
               * @param two_theta The value of the "2theta" Axe.
               */
              Vertical(double const & omega, double const & two_theta);

              virtual ~Vertical(void); //!< Default destructor.

              /** 
               * @brief Set the angles of the eulerian4CD::Vertical geometry.
               * 
               * @param omega The value of the "omega" Axe.
               * @param two_theta The value of the "2theta" Axe.
               */
              void setAngles(double const & omega, double const & two_theta);

              /** 
               * @brief Set a TwoC::Vertical Geometry from a eulerian4C::Vertical Geometry.
               * @param E4C The eulerian4C geometry.
               * @throw HKLException if the "chi" and "phi" axes are != 0 
               */
              void setFromGeometry(eulerian4C::Vertical const & E4C) throw (HKLException);

              /** 
               * @brief Set a TwoC::Vertical Geometry from a kappa4C::Vertical Geometry.
               * @param K4C The kappa4C geometry.
               * @throw HKLException if the "kappa" and "kphi" axes are != 0 
               */
              void setFromGeometry(kappa4C::Vertical const & K4C) throw (HKLException);

              /** 
               * @brief Set a TwoC::Vertical Geometry from a kappa6C Geometry.
               * @param K6C 
               * @throw HKLException if the "gamma", "mu", "kappa" and "kphi" axes are != 0 
               */
              void setFromGeometry(Kappa6C const & K6C) throw (HKLException);
            };

        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_TWOC_H_
