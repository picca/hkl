#include "diffractometer_kappa.h"

namespace hkl
  {
  namespace diffractometer
    {

    Kappa::Kappa(double alpha) : Diffractometer()
    {
      addParameter("alpha", alpha, "The alpha angle of the kappa geometry.");
    }

    Kappa::~Kappa(void)
    {}

  } // namespace diffractometer
} // namespace hkl
