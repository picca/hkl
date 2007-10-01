
#include "geometry_kappa.h"

namespace hkl
  {

  namespace geometry
    {

    Kappa::Kappa(const std::string & name, const std::string & description, double alpha) :
        Geometry(name, description),
        _alpha(alpha)
    {
    }

    Kappa::~Kappa()
    {
    }

    /**
     * @brief print the Kappa into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    std::ostream & Kappa::printToStream(std::ostream & flux) const
      {
        flux.precision(3);
        flux << " alpha : " << _alpha << std::endl;
        Geometry::printToStream(flux);
        return flux;
      }

  } // namespace hkl::geometry

} // namespace hkl
