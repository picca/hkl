#include "geometry_kappa.h"

namespace hkl
  {
  namespace geometry
    {

    Kappa::Kappa(MyString const & name, MyString const & description, double alpha) :
        Geometry(name, description),
        _alpha(alpha)
    {}

    Kappa::~Kappa(void)
    {}

    Kappa &
    Kappa::operator=(Kappa const & geometry)
    {
      Geometry::operator=(geometry);
      _alpha = geometry._alpha;
      return *this;
    }

    ostream &
    Kappa::printToStream(ostream & flux) const
      {
        flux.precision(3);
        flux << " alpha : " << _alpha << endl;
        Geometry::printToStream(flux);
        return flux;
      }

    ostream &
    Kappa::toStream(ostream & flux) const
      {
        Geometry::toStream(flux);
        flux << " " << _alpha << endl;
        return flux;
      }

    istream &
    Kappa::fromStream(istream & flux)
    {
      Geometry::fromStream(flux);
      flux >> _alpha;
      return flux;
    }

  } // namespace geometry
} // namespace hkl
