#include "geometry_kappa.h"

namespace hkl {
    namespace geometry {

        Kappa::Kappa(double alpha) :
          Geometry(),
          m_alpha(alpha)
        {}

        Kappa::~Kappa(void)
          {}

        Kappa &
        Kappa::operator=(Kappa const & geometry)
          {
            Geometry::operator=(geometry);
            m_alpha = geometry.m_alpha;
            return *this;
          }

        bool
        Kappa::isValid(void) const throw (HKLException)
          {
            bool valid = Geometry::isValid();
            if (fabs(m_alpha) > constant::math::epsilon_0)
                return true;
            else
                HKLEXCEPTION("The geometry is not valid.", "Please set a non null alpha.");
          }

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
