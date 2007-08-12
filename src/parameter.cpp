
#include "parameter.h"
#include "value.h"

namespace hkl {

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
  Range(min, current, max)
{
  // Bouml preserved body begin 00025302
  // Bouml preserved body end 00025302
}

/*!
 * \brief Are two Parameter equals ?
 * \param parameter the hkl::Parameter to compare with.
 */

bool Parameter::operator==(const hkl::Parameter & parameter) const 
{
  // Bouml preserved body begin 00025482
      return ObjectReadOnly::operator==(parameter)
        && Range::operator==(parameter);
  // Bouml preserved body end 00025482
}

/*!
 * \brief print the Parameter into a flux
 * \param flux The stream to print into.
 */
ostream & Parameter::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00025502
      ObjectReadOnly::printToStream(flux);
      Range::printToStream(flux);
      return flux;
  // Bouml preserved body end 00025502
}

/*!
 * \brief Save the Parameter into a stream.
 * \param flux the stream to save the Parameter into.
 * \return The stream with the Parameter.
 */
ostream & Parameter::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00025582
      ObjectReadOnly::toStream(flux);
      Range::toStream(flux);
      return flux;
  // Bouml preserved body end 00025582
}

/*!
 * \brief Restore a Parameter from a stream.
 * \param flux The stream containing the Parameter to restore.
 * @todo call update_observers or not ?
 */
istream & Parameter::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00025602
      ObjectReadOnly::fromStream(flux);
      Range::fromStream(flux);
      return flux;
  // Bouml preserved body end 00025602
}


} // namespace hkl
