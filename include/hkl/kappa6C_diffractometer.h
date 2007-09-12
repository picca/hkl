#ifndef _KAPPA6C_DIFFRACTOMETER_H
#define _KAPPA6C_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "kappa6C_mode.h"
#include "kappa6C_pseudoaxeengine.h"
#include "kappa6C_geometry.h"

namespace hkl
  {

  namespace kappa6C
    {

    class Diffractometer : public hkl::DiffractometerTemp<hkl::kappa6C::Geometry>
      {
      public:
        Diffractometer(double alpha);

        virtual ~Diffractometer();

      };

  } // namespace hkl::kappa6C

} // namespace hkl
#endif
