#include "angleconfiguration_eulerian4C.h"

namespace hkl {
  namespace angleConfiguration {

    Eulerian4C::Eulerian4C()
    {
      addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
      addSampleAxe(Axe("chi", svector(1., 0., 0.), 1));
      addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));

      addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
    }

    Eulerian4C::~Eulerian4C()
    {
    }

  } // namespace angleConfiguration
} // namespace hkl
