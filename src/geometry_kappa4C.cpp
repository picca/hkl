#include "geometry_kappa4C.h"

namespace hkl {
  namespace geometry {

    Kappa4C::Kappa4C(void)
    {
      addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
      addSampleAxe(Axe("kappa", svector(1., 0., 0.), 1));
      addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));
      addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
    }

    Kappa4C::~Kappa4C(void)
    {}

  } // namespace geometry
} // namespace hkl
