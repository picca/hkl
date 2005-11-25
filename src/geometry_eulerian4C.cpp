#include "geometry_eulerian4C.h"
#include "pseudoaxe_eulerian4C.h"

namespace hkl {
  namespace geometry {
    
    Eulerian4C::Eulerian4C(void)
      : Geometry()
    {
      addSampleAxe(Axe("omega", svector(0., 1., 0.), -1));
      addSampleAxe(Axe("chi", svector(1., 0., 0.), 1));
      addSampleAxe(Axe("phi", svector(0., 1., 0.), -1));

      addDetectorAxe(Axe("2theta", svector(0., 1., 0.), -1));
    }

    Eulerian4C::Eulerian4C(Geometry const & geometry)
      : Geometry(geometry)
    {}

    Eulerian4C::~Eulerian4C(void)
    {}

  } // namespace geometry
} // namespace hkl
