
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

} // namespace hkl
