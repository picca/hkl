
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

    /**
     * @brief print on a stream the content of the Kappa
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    std::ostream & Kappa::toStream(std::ostream & flux) const
      {
        Geometry::toStream(flux);
        flux << " " << _alpha << std::endl;
        return flux;
      }

    /**
     * @brief restore the content of the Kappa from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    std::istream & Kappa::fromStream(std::istream & flux)
    {
      Geometry::fromStream(flux);
      flux >> _alpha;
      return flux;
    }


  } // namespace hkl::geometry

} // namespace hkl
