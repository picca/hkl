
#include "object.h"

namespace hkl
  {

  /**
   * @brief Another constructor
   * @param name The name of the Object
   * @param description The description of the Object
   * @throw HKLException if the name or the description is empty.
   */
  ObjectBase::ObjectBase(const std::string & name, const std::string & description) throw(hkl::HKLException)
  {
    this->_set_name(name);
    this->_set_description(description);
  }

  /**
   * @brief Copy constructor
   * @param source the ObjectBase to copy.
   */
  ObjectBase::ObjectBase(const hkl::ObjectBase & source) :
      _name(source._name),
      _description(source._description)
  {
  }

  /**
   * \brief Are two Object equals ?
   * \param object the Object to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool ObjectBase::operator==(const hkl::ObjectBase & object) const
    {
      return _name == object._name
             && _description == object._description;
    }

  /**
   * @brief Get the name of the ObjectBase.
   * @return a string ref with the name of the ObjectBase.
   */
  const std::string & ObjectBase::_get_name() const
    {
      return _name;
    }

  /**
   * @brief Set the name of the BaseObject.
   * @param name The new name of the BaseObject.
   */
  void ObjectBase::_set_name(const std::string & name) throw(hkl::HKLException)
  {
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
  }

  /**
   * @brief Get the description of the ObjectBase.
   * @return a string ref with the name of the ObjectBase.
   */
  const std::string & ObjectBase::_get_description() const
    {
      return _description;
    }

  /**
   * @brief Set the description of the Object.
   * @param description The new BaseObject description.
   */
  void ObjectBase::_set_description(const std::string & description) throw(hkl::HKLException)
  {
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
  }

  /**
   * @brief print the ObjectBase into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & ObjectBase::printToStream(std::ostream & flux) const
    {
      flux
      << "\"" << _name << "\""
      << " \"" << _description << "\"";

      return flux;
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
  }

  /**
   * @brief Copy constructor
   * @param source the ObjectReadOnly to copy.
   */
  ObjectReadOnly::ObjectReadOnly(const hkl::ObjectReadOnly & source) :
      ObjectBase(source)
  {
  }

  /**
   * @brief Get the name of the ObjectReadOnly.
   * @return a string reference with the name of the ObjectReadOnly.
   */
  const std::string & ObjectReadOnly::get_name() const
    {
      return ObjectBase::_get_name();
    }

  /**
   * @brief Get the description of the ObjectReadOnly.
   * @return a string reference with the description of the ObjectReadOnly.
   */
  const std::string & ObjectReadOnly::get_description() const
    {
      return ObjectBase::_get_description();
    }

  /**
   * @brief The default constructor.
   */
  Object::Object() :
      ObjectReadOnly("unknown", "unknown")
  {
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
  }

  /**
   * @brief Copy constructor
   * @param source the Object to copy.
   */
  Object::Object(const hkl::Object & source) :
      ObjectReadOnly(source)
  {
  }

  /**
   * @brief Set the name of the Object.
   * @param name
   */
  void Object::set_name(const std::string & name)
  {
    ObjectReadOnly::_set_name(name);
  }

  /**
   * @brief Set the name of the Object.
   * @param description
   */
  void Object::set_description(const std::string & description) throw(hkl::HKLException)
  {
    ObjectReadOnly::_set_description(description);
  }


} // namespace hkl
