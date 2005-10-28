#ifndef _GEOMETRY_EULERIAN4C_H_
#define _GEOMETRY_EULERIAN4C_H_

#include "geometry.h"

namespace hkl {
  namespace geometry {

    /**
     * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
     */
    class Eulerian4C : public Geometry
    {
      public:

        /**
         * \brief The default constructor
         */
        Eulerian4C(void);

        /**
         * \Copy constructor from a Geometry.
         */
        Eulerian4C(Geometry const &);

        /**
         * \brief The destructor
         */
        virtual ~Eulerian4C(void);
    };

  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN4C_H_
