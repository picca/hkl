#ifndef _GEOMETRY_KAPPA4C_H_
#define _GEOMETRY_KAPPA4C_H_

#include "geometry.h"

namespace hkl {
  namespace geometry {

    /**
     * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
     */
    class Kappa4C : public Geometry
    {
      public:

        /**
         * \brief The default constructor
         */
        Kappa4C(void);

        /**
         * \brief The destructor
         */
        virtual ~Kappa4C(void);
    };

  } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA4C_H_
