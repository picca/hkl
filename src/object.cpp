#include "object.h"

#include <sstream>

namespace hkl
  {

  BaseObject::BaseObject(MyString const & name, MyString const & description)  throw (HKLException)
  {
    _set_name(name);
    _set_description(description);
  }

  void
  BaseObject::_set_name(MyString const & name) throw (HKLException)
  {
    //verification that the name is a valid name.
    if (name.size())
      m_name = name;
    else
      {
        ostringstream reason;
        reason << "Can not set the " << typeid(*this).name() << " name with an empty name";
        HKLEXCEPTION(reason.str(),
                     "Please set with a non empty name");
      }
  }

  void
  BaseObject::_set_description(MyString const & description) throw (HKLException)
  {
    //verification that the description is a valid description.
    if (description.size())
      m_description = description;
    else
      {
        ostringstream reason;
        reason << "Can not set the " << typeid(*this).name() << " description with an empty description";
        HKLEXCEPTION(reason.str(),
                     "Please set with a non empty description");
      }
  }

  bool
  BaseObject::operator == (BaseObject const & object) const
    {
      return m_name == object.m_name
             && m_description == object.m_description;
    }

  ostream &
  BaseObject::printToStream(ostream & flux) const
    {
      flux
      << " Name: " << m_name << endl
      << " Description: " << m_description << endl;

      return flux;
    }

  ostream &
  BaseObject::toStream(ostream & flux) const
    {
      m_name.toStream(flux);
      m_description.toStream(flux);

      return flux;
    }

  istream &
  BaseObject::fromStream(istream & flux)
  {
    m_name.fromStream(flux);
    m_description.fromStream(flux);

    return flux;
  }

  ObjectReadOnly::ObjectReadOnly(MyString const & name, MyString const & description) throw (HKLException) :
      BaseObject(name, description)
  {}

  Object::Object(void)
      : ObjectReadOnly("no description", "no description")
  {}

  Object::Object(MyString const & name) throw (HKLException)
      : ObjectReadOnly(name, "no description")
  {}

  Object::Object(MyString const & name, MyString const & description) throw (HKLException) :
      ObjectReadOnly(name, description)
  {}

} // namespace hkl
