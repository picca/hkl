
#include "diffractometer.h"

#include "portability.h"
namespace hkl
  {

  Diffractometer::Diffractometer(const std::string & name, const std::string & description) :
      HKLObject(name, description)
  {
  }

  Diffractometer::~Diffractometer()
  {
  }

  /**
   * \brief Are two Diffractometer equals ?
   * \param diffractometer the hkl::Diffractometer to compare with.
   * \return true if both are equals false otherwise.
   */
  bool Diffractometer::operator==(const hkl::Diffractometer & diffractometer) const
    {
      return HKLObject::operator==(diffractometer)
             && *_geometry == *(diffractometer._geometry)
             && *_samples == *(diffractometer._samples)
             && _modes == diffractometer._modes
             && _pseudoAxeEngines == diffractometer._pseudoAxeEngines;
    }

  /**
   * @brief print the Diffractometer into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Diffractometer::printToStream(std::ostream & flux) const
    {
      flux << std::endl;
      flux << "Diffractometer: \"" << get_name() << "\"" << std::endl;
      HKLObject::printToStream(flux);
      _geometry->printToStream(flux);
      _samples->printToStream(flux);
      _modes.printToStream(flux);
      _pseudoAxeEngines.printToStream(flux);

      return flux;
    }

  /**
   * @brief print on a stream the content of the Diffractometer
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Diffractometer::toStream(std::ostream & flux) const
    {
      flux << " " << HKL_VERSION;
      HKLObject::toStream(flux);
      _geometry->toStream(flux);
      _samples->toStream(flux);
      _modes.toStream(flux);
      _pseudoAxeEngines.toStream(flux);

      return flux;
    }

  /**
   * @brief restore the content of the Diffractometer from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Diffractometer::fromStream(std::istream & flux)
  {
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
  }


} // namespace hkl
