#include "object.h"

namespace hkl {
  
  Object::Object(void)
  {}

  Object::Object(std::string const & name) :
    m_name(name),
    m_description("")
  {}

  Object::Object(std::string const & name, std::string const & description) :
    m_name(name),
    m_description(description)
  {}

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

  std::ostream & 
  Object::printToStream(std::ostream & flux) const
  { 
    flux
      << " Name: " << m_name << std::endl
      << " Description: " << m_description << std::endl;

    return flux;
  }

} // namespace hkl

std::ostream &
operator << (std::ostream & flux, hkl::Object const & object)
{
  return object.printToStream(flux);
}
