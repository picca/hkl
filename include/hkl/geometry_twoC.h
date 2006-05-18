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
               * @brief Set a TwoC::Vertical Geometry from an other Geometry.
               * @param geometry The Geometry.
               * @param stric if stric geometry are equivalent, if not update only the necessary axes.
               * @throw HKLException depending of the true type of the geometry. 
               *
               * The stric parameter is use to specifi witch axxes must be update from the geometry
               * and if exception must be throw depending of the value of the axes.
               * stric = true
               * eulerian4C::Vertical(omega = 30, chi = 10, phi = 15, two_theta = 60) -> throw an exception because chi <> phi <> 0
               * but if stric = false no exception is throw.
               */
              void setFromGeometry(Geometry const & geometry, bool const & strict) throw (HKLException);
            };

        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_TWOC_H_
