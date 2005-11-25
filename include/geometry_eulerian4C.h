#ifndef _GEOMETRY_EULERIAN4C_H_
#define _GEOMETRY_EULERIAN4C_H_

#include "geometry.h"

using namespace std;

namespace hkl {
  namespace geometry {

    /**
     * \brief A Geometry for a the eulerian 4 circle soleil generic diffractometer.
     */
    class Eulerian4C : public Geometry
    { 
      public:

        Eulerian4C(void); //!< Default constructor.

        Eulerian4C(Geometry const & geometry); //!< Copy Constructor.

        virtual ~Eulerian4C(void); //!< Default destructor.
    };

  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
