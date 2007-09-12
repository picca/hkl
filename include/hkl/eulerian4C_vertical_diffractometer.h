#ifndef _EULERIAN4C_VERTICAL_DIFFRACTOMETER_H
#define _EULERIAN4C_VERTICAL_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "eulerian4C_vertical_mode.h"
#include "eulerian4C_vertical_geometry.h"

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      class Diffractometer : public hkl::DiffractometerTemp<hkl::eulerian4C::vertical::Geometry>
        {
        public:
          Diffractometer();

          virtual ~Diffractometer();

        };

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
#endif
