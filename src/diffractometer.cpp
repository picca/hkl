
#include "diffractometer.h"

#include "portability.h"
namespace hkl {

Diffractometer::Diffractometer(const std::string & name, const std::string & description) :
  HKLObject(name, description) 
{
  // Bouml preserved body begin 00037302
  // Bouml preserved body end 00037302
}

Diffractometer::~Diffractometer() 
{
  // Bouml preserved body begin 00037382
  // Bouml preserved body end 00037382
}

/**
 * \brief Are two Diffractometer equals ?
 * \param diffractometer the hkl::Diffractometer to compare with.
 * \return true if both are equals false otherwise.
 */
bool Diffractometer::operator==(const hkl::Diffractometer & diffractometer) const 
{
  // Bouml preserved body begin 00037682
      return HKLObject::operator==(diffractometer)
             && *_geometry == *(diffractometer._geometry)
             && *_samples == *(diffractometer._samples)
             && _modes == diffractometer._modes
             && _pseudoAxeEngines == diffractometer._pseudoAxeEngines;
  // Bouml preserved body end 00037682
}

/**
 * @brief print the Diffractometer into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & Diffractometer::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00037702
      flux << std::endl;
      flux << "Diffractometer: \"" << get_name() << "\"" << std::endl;
      HKLObject::printToStream(flux);
      _geometry->printToStream(flux);
      _samples->printToStream(flux);
      _modes.printToStream(flux);
      _pseudoAxeEngines.printToStream(flux);
      
      return flux;
  // Bouml preserved body end 00037702
}

/**
 * @brief print on a stream the content of the Diffractometer
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Diffractometer::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00037782
      flux << " " << HKL_VERSION;
      HKLObject::toStream(flux);
      _geometry->toStream(flux);
      _samples->toStream(flux);
      _modes.toStream(flux);
      _pseudoAxeEngines.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00037782
}

/**
 * @brief restore the content of the Diffractometer from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Diffractometer::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00037802
      unsigned int version;
      
      flux >> version;
      if (version == HKL_VERSION)
        {
          HKLObject::fromStream(flux);
          _geometry->fromStream(flux);
          _samples->fromStream(flux);
          _modes.fromStream(flux);
          _pseudoAxeEngines.fromStream(flux);
        }
      return flux;
  // Bouml preserved body end 00037802
}


} // namespace hkl
