#include "diffractometer_kappa.h"

namespace hkl {
    namespace diffractometer {

        Kappa::Kappa(double alpha) : Diffractometer()
        {
          addParameter("alpha");
          setParameterValue("alpha", alpha);
        }

        Kappa::~Kappa(void)
          {}

    } // namespace diffractometer
} // namespace hkl