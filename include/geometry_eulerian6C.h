#ifndef _GEOMETRY_EULERIAN6C_H_
#define _GEOMETRY_EULERIAN6C_H_

#include "geometry.h"

namespace hkl {
  namespace geometry {

    /**
     * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
     */
    class Eulerian6C : public Geometry
    {
      public:

        /**
         * \brief The default constructor
         */
        Eulerian6C(void);

        /**
         * \brief The destructor
         */
        virtual ~Eulerian6C(void);
    };

  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_EULERIAN6C_H_
