
#include "object.h"

namespace hkl {

/**
 * @brief Another constructor
 * @param name The name of the Object
 * @param description The description of the Object
 * @throw HKLException if the name or the description is empty.
 */
ObjectBase::ObjectBase(const std::string & name, const std::string & description) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00023502
      _set_name(name);
      _set_description(description);
  // Bouml preserved body end 00023502
}

/**
 * @brief Copy constructor
 * @param source the ObjectBase to copy.
 */
ObjectBase::ObjectBase(const hkl::ObjectBase & source) :
  _name(source._name),
  _description(source._description) 
{
  // Bouml preserved body begin 00023402
  // Bouml preserved body end 00023402
}

/**
 * \brief Are two Object equals ?
 * \param object the Object to compare with.
 * \return true if both are equals flase otherwise.
 */
bool ObjectBase::operator==(const hkl::ObjectBase & object) const 
{
  // Bouml preserved body begin 00023782
      return _name == object._name
        && _description == object._description;
  // Bouml preserved body end 00023782
}

/**
 * @brief Get the name of the ObjectBase.
 * @return a string ref with the name of the ObjectBase.
 */
const std::string & ObjectBase::_get_name() const 
{
  // Bouml preserved body begin 00024102
      return _name;
  // Bouml preserved body end 00024102
}

/**
 * @brief Set the name of the BaseObject.
 * @param name The new name of the BaseObject.
 */
void ObjectBase::_set_name(const std::string & name) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00023802
      //verification that the name is a valid name.
      if (name.size())
        _name = name;
      else
      {
          std::ostringstream reason;
          reason << "Can not set the " << typeid(*this).name() << " name with an empty name";
          HKLEXCEPTION(reason.str(),
                       "Please set with a non empty name");
      }
  // Bouml preserved body end 00023802
}

/**
 * @brief Get the description of the ObjectBase.
 * @return a string ref with the name of the ObjectBase.
 */
const std::string & ObjectBase::_get_description() const 
{
  // Bouml preserved body begin 00024182
      return _description;
  // Bouml preserved body end 00024182
}

/**
 * @brief Set the description of the Object.
 * @param description The new BaseObject description.
 */
void ObjectBase::_set_description(const std::string & description) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00023882
      //verification that the name is a valid name.
      if (description.size())
        _description = description;
      else
      {
          std::ostringstream reason;
          reason << "Can not set the " << typeid(*this).name() << " description with an empty description";
          HKLEXCEPTION(reason.str(),
                       "Please set with a non empty description");
      }
  // Bouml preserved body end 00023882
}

/**
 * @brief print the ObjectBase into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & ObjectBase::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00023A02
      flux
      << " Name: " << _name << std::endl
      << " Description: " << _description << std::endl;
      
      return flux;
  // Bouml preserved body end 00023A02
}

/**
 * @brief print on a stream the content of the ObjectBase
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & ObjectBase::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00023902
      _name.toStream(flux);
      _description.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00023902
}

/**
 * @brief restore the content of the ObjectBase from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & ObjectBase::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00023982
      _name.fromStream(flux);
      _description.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00023982
}

/**
 * @brief The default constructor.
 * @param name The name of the Object
 * @param description The description of the Object
 * @throw HKLException if the name or the description is empty.
 */
ObjectReadOnly::ObjectReadOnly(const std::string & name, const std::string & description) throw(hkl::HKLException) :
  ObjectBase(name, description) 
{
  // Bouml preserved body begin 00023A82
  // Bouml preserved body end 00023A82
}

/**
 * @brief Copy constructor
 * @param source the ObjectReadOnly to copy.
 */
ObjectReadOnly::ObjectReadOnly(const hkl::ObjectReadOnly & source) :
  ObjectBase(source) 
{
  // Bouml preserved body begin 00023B82
  // Bouml preserved body end 00023B82
}

/**
 * @brief Get the name of the ObjectReadOnly.
 * @return a string reference with the name of the ObjectReadOnly.
 */
const std::string & ObjectReadOnly::get_name() const 
{
  // Bouml preserved body begin 00023C02
      return ObjectBase::_get_name();
  // Bouml preserved body end 00023C02
}

/**
 * @brief Get the description of the ObjectReadOnly.
 * @return a string reference with the description of the ObjectReadOnly.
 */
const std::string & ObjectReadOnly::get_description() const 
{
  // Bouml preserved body begin 00023C82
      return ObjectBase::_get_description();
  // Bouml preserved body end 00023C82
}

/**
 * @brief The default constructor.
 */
Object::Object() :
  ObjectReadOnly("unknown", "unknown") 
{
  // Bouml preserved body begin 00023F82
  // Bouml preserved body end 00023F82
}

/**
 * @brief The default constructor.
 * @param name The name of the Object
 * @param description The description of the Object
 * @throw HKLException if the name or the description is empty.
 */
Object::Object(const std::string & name, const std::string & description) throw(hkl::HKLException) :
  ObjectReadOnly(name, description) 
{
  // Bouml preserved body begin 00023D02
  // Bouml preserved body end 00023D02
}

/**
 * @brief Copy constructor
 * @param source the Object to copy.
 */
Object::Object(const hkl::Object & source) :
  ObjectReadOnly(source) 
{
  // Bouml preserved body begin 00023E02
  // Bouml preserved body end 00023E02
}

/**
 * @brief Set the name of the Object.
 * @param name
 */
void Object::set_name(const std::string & name) 
{
  // Bouml preserved body begin 00023E82
      ObjectReadOnly::_set_name(name);
  // Bouml preserved body end 00023E82
}

/**
 * @brief Set the name of the Object.
 * @param description
 */
void Object::set_description(const std::string & description) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00024002
      ObjectReadOnly::_set_description(description);
  // Bouml preserved body end 00024002
}


} // namespace hkl
