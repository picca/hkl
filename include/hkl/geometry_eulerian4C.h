#ifndef _GEOMETRY_EULERIAN4C_H_
#define _GEOMETRY_EULERIAN4C_H_

#include "geometry.h"
//#include "geometry_kappa4C.h"

using namespace std;

namespace hkl {
    namespace geometry {

        //forward declaration
        namespace kappa4C {
          class Vertical;
        }
        class Kappa6C;

        namespace eulerian4C {
        /**
         * \brief A Geometry for a the eulerian 4 circle soleil generic diffractometer.
         */
        class Vertical : public Geometry
        { 
        public:

          Vertical(void); //!< Default constructor.

          Vertical(Geometry const & geometry); //!< Copy Constructor.

          /*! 
            \brief Constructor	  
            \param omega The value of the "omega" Axe.
            \param chi The value of the "chi" Axe 
            \param phi The value of the "phi" Axe 
            \param two_theta The value of the "2theta" Axe 

            \return A new hkl::geometry::Eulerian4C Geometry.
            */
          Vertical(double omega, double chi, double phi, double two_theta);

          virtual ~Vertical(void); //!< Default destructor.

          void setFromGeometry(kappa4C::Vertical const & K4C);

          void setFromGeometry(Kappa6C const & K6C);
        };
        
        } // namespace eulerian4C
    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
