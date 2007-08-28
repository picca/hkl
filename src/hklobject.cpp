
#include "hklobject.h"

namespace hkl {

/**
 * @brief The default constructor
 * @param name The name of the HKLObject.
 * @param description The description of the HKLObject.
 * @throw HKLException if the name and/or the description are wrong. 
 */
HKLObject::HKLObject(const std::string & name, const std::string & description) throw(hkl::HKLException):
  ObjectReadOnly(name, description)  
{
  // Bouml preserved body begin 00026E02
  // Bouml preserved body end 00026E02
}

hkl::ParameterList & HKLObject::parameters() 
{
  // Bouml preserved body begin 00026E82
      return _parameters;
  // Bouml preserved body end 00026E82
}

/**
 * \brief Are two HKLObject equals ?
 * \param hklObject the hkl::HKLObject to compare with.
 * \return true if both are equals flase otherwise.
 */
bool HKLObject::operator==(const hkl::HKLObject & hklObject) const 
{
  // Bouml preserved body begin 00026F02
      return ObjectReadOnly::operator==(hklObject)
        && _parameters == hklObject._parameters;
  // Bouml preserved body end 00026F02
}

/**
 * @brief print the HKLObject into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & HKLObject::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00026F82
      ObjectReadOnly::printToStream(flux);
      flux << std::endl << _parameters;
      
      return flux;
  // Bouml preserved body end 00026F82
}

/**
 * @brief print on a stream the content of the HKLObject
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & HKLObject::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00027002
      ObjectReadOnly::toStream(flux);
      _parameters.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00027002
}

/**
 * @brief restore the content of the HKLObject from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & HKLObject::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00027082
      ObjectReadOnly::fromStream(flux);
      _parameters.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00027082
}


} // namespace hkl
