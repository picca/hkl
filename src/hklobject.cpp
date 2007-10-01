
#include "hklobject.h"

namespace hkl
  {

  /**
   * @brief The default constructor
   * @param name The name of the HKLObject.
   * @param description The description of the HKLObject.
   * @throw HKLException if the name and/or the description are wrong.
   */
  HKLObject::HKLObject(const std::string & name, const std::string & description) throw(hkl::HKLException):
      ObjectReadOnly(name, description)
  {
  }

  hkl::ParameterList & HKLObject::parameters()
  {
    return _parameters;
  }

  /**
   * \brief Are two HKLObject equals ?
   * \param hklObject the hkl::HKLObject to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool HKLObject::operator==(const hkl::HKLObject & hklObject) const
    {
      return ObjectReadOnly::operator==(hklObject)
             && _parameters == hklObject._parameters;
    }

  /**
   * @brief print the HKLObject into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & HKLObject::printToStream(std::ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      flux << std::endl << _parameters;

      return flux;
    }

} // namespace hkl
