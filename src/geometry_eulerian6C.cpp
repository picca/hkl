#include "geometry_eulerian6C.h"

namespace hkl {
  namespace geometry {

    Eulerian6C::Eulerian6C(void)
    {
      addSampleAxe(Axe("nu", svector(0., 0., 1.), 1));
      addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
      addSampleAxe(Axe("chi", svector(1., 0., 0.), 1));
      addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));

      addDetectorAxe(Axe("gamma", svector(0., 0., 1.), 1));
      addDetectorAxe(Axe("delta", svector(0., 1., 0.), -1));
    }

    Eulerian6C::~Eulerian6C(void)
    {}

  } // namespace geometry
} // namespace hkl
