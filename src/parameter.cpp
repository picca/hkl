
#include "parameter.h"
#include "value.h"

namespace hkl
  {

  /**
   * @brief The default constructor
   * @param name The name std::string of the Parameter.
   * @param description The description std::string of the Parameter.
   * @param min the minimum hkl::Value of the Parameter.
   * @param current The current hkl::Value of the Parameter.
   * @param max The maximum hkl::Value of the Parameter.
   * @throw HKLException if the min <= current <= max is not verify.
   */
  Parameter::Parameter(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException) :
      ObjectReadOnly(name, description),
      Range(min, current, current, max)
  {
  }

  /*!
   * \brief Are two Parameter equals ?
   * \param parameter the hkl::Parameter to compare with.
   */

  bool Parameter::operator==(const hkl::Parameter & parameter) const
    {
      return ObjectReadOnly::operator==(parameter)
             && Range::operator==(parameter);
    }

  /*!
   * \brief print the Parameter into a flux
   * \param flux The stream to print into.
   */
  std::ostream & Parameter::printToStream(std::ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      flux << " ";
      Range::printToStream(flux);
      return flux;
    }

} // namespace hkl
