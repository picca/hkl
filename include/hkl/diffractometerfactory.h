#ifndef _DIFFRACTOMETERFACTORY_H
#define _DIFFRACTOMETERFACTORY_H


#include "twoC_vertical_diffractometer.h"
#include "eulerian4C_vertical_diffractometer.h"
#include "kappa4C_vertical_diffractometer.h"
#include "eulerian6C_diffractometer.h"
#include "kappa6C_diffractometer.h"

namespace hkl
  {
  class Diffractometer;
}

namespace hkl
  {

  enum DiffractometerType
  {
    DIFFRACTOMETER_TWOC_VERTICAL,
    DIFFRACTOMETER_EULERIAN4C_VERTICAL,
    DIFFRACTOMETER_KAPPA4C_VERTICAL,
    DIFFRACTOMETER_EULERIAN6C,
    DIFFRACTOMETER_KAPPA6C
  };
  class DiffractometerFactory
    {
    public:
      DiffractometerFactory();

      virtual ~DiffractometerFactory();

      /**
       * @brief Create a new reflection.
       * @param type The hkl::DiffractometerType of the Diffractometer to create.
       * @param parameter A double use to build the Diffractometer.
       * @return The created Diffractometer.
       *
       * This parameter has no effect for an Eulerian diffractometer.
       * But correspond to the alpha angle of the Kappa Geometry for the Kappa diffractometers.
       */

      hkl::Diffractometer * create(hkl::DiffractometerType type, double parameter);

    };

} // namespace hkl
#endif
