#ifndef _GEOMETRY_EULERIAN4C_H_
#define _GEOMETRY_EULERIAN4C_H_

#include "geometry.h"
#include "geometry_kappa4C.h"

using namespace std;

namespace hkl {
    namespace geometry {

        class Kappa4C;

        /**
         * \brief A Geometry for a the eulerian 4 circle soleil generic diffractometer.
         */
        class Eulerian4C : public Geometry
        { 
        public:

          Eulerian4C(void); //!< Default constructor.

          Eulerian4C(Geometry const & geometry); //!< Copy Constructor.

          /*! 
            \brief Constructor	  
            \param omega The value of the "omega" Axe.
            \param chi The value of the "chi" Axe 
            \param phi The value of the "phi" Axe 
            \param two_theta The value of the "2theta" Axe 

            \return A new hkl::geometry::Eulerian4C Geometry.
            */
          Eulerian4C(double omega, double chi, double phi, double two_theta);

          virtual ~Eulerian4C(void); //!< Default destructor.

          void setFromK4C(Kappa4C const & K4C);
        };

    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
