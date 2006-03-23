#include "geometry_kappa.h"

namespace hkl {
    namespace geometry {

        Kappa::Kappa(double alpha) :
          Geometry(),
          m_alpha(alpha)
        {}

        Kappa::~Kappa(void)
          {}

        ostream &
        Kappa::printToStream(ostream & flux) const
          {
            flux.precision(3);
            flux << " alpha : " << m_alpha << endl;
            Geometry::printToStream(flux);
            return flux;
          }

        ostream &
        Kappa::toStream(ostream & flux) const
          {
            Geometry::toStream(flux);
            flux << " " << m_alpha << endl;
            return flux;    
          }

        istream &
        Kappa::fromStream(istream & flux)
          {
            Geometry::fromStream(flux);
            flux >> m_alpha;
            return flux;
          }

    } // namespace geometry
} // namespace hkl
