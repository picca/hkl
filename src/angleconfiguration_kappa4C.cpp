#include "angleconfiguration_kappa4C.h"

namespace hkl {
  namespace angleConfiguration {

    Kappa4C::Kappa4C()
    {
      addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
      addSampleAxe(Axe("kappa", svector(1., 0., 0.), 1));
      addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));
      addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
    }

    Kappa4C::~Kappa4C()
    {
    }

  } // namespace angleConfiguration
} // namespace hkl
