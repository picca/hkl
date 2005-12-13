#include "object.h"

#include <sstream>

namespace hkl {
  
  Object::Object(void)
    : m_name("no name"),
      m_description("no description")
  {}

  Object::Object(string const & name) throw (HKLException)
    : m_description("no description")
  {
    set_name(name);
  }

  Object::Object(string const & name, string const & description) throw (HKLException)
  {
    set_name(name);
    set_description(description);
  }

  Object::Object(Object const & object) :
    m_name(object.m_name),
    m_description(object.m_description)
  {}

  Object::~Object(void)
  {}

  bool
  Object::operator == (Object const & object) const
  {
    return m_name == object.m_name
      && m_description == object.m_description;
  }

  void
  Object::set_name(string const & name) throw (HKLException)
  {
    //verification that the name is a valid name.
    if (name.size() == 0)
      throw(HKLException("The name is not a valid name",
                         "Please change the name",
                         "Object::set_name"));
    else
      m_name = name;
  }

  void
  Object::set_description(string const & description) throw (HKLException)
  {
    //verification that the name is a valid name.
    if (description.size() == 0)
      throw(HKLException("The description is not a valid description",
                         "Please change the description",
                         "Object::set_description"));
    else
      m_description = description;
  }
  
  ostream & 
  Object::printToStream(ostream & flux) const
  { 
    flux
      << " Name: " << m_name << endl
      << " Description: " << m_description << endl;

    return flux;
  }

  ostream &
  Object::toStream(ostream & flux) const throw (HKLException)
  {
    flux << char(30) << m_name << char(30) << m_description << char(30) << endl;
    
    return flux;    
  }

  istream &
  Object::fromStream(istream & flux)
  {
    string junk;

    getline(flux, junk, char(30));
    getline(flux, m_name, char(30));
    getline(flux, m_description, char(30));

    return flux;
  }
  
} // namespace hkl

ostream &
operator << (ostream & flux, hkl::Object const & object)
{
  return object.printToStream(flux);
}
