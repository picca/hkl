
#include "geometry_kappa.h"

namespace hkl {

namespace geometry {

Kappa::Kappa(const std::string & name, const std::string & description, double alpha) :
  Geometry(name, description),
  _alpha(alpha)   
{
  // Bouml preserved body begin 0002B502
  // Bouml preserved body end 0002B502
}

Kappa::~Kappa() 
{
  // Bouml preserved body begin 00034182
  // Bouml preserved body end 00034182
}

/**
 * @brief print the Kappa into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & Kappa::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0002B682
      flux.precision(3);
      flux << " alpha : " << _alpha << endl;
      Geometry::printToStream(flux);
      return flux;
  // Bouml preserved body end 0002B682
}

/**
 * @brief print on a stream the content of the Kappa
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & Kappa::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 0002B702
      Geometry::toStream(flux);
      flux << " " << _alpha << endl;
      return flux;
  // Bouml preserved body end 0002B702
}

/**
 * @brief restore the content of the Kappa from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & Kappa::fromStream(istream & flux) 
{
  // Bouml preserved body begin 0002B782
      Geometry::fromStream(flux);
      flux >> _alpha;
      return flux;
  // Bouml preserved body end 0002B782
}


} // namespace hkl::geometry

} // namespace hkl
