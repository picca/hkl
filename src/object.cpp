#include "object.h"

Object::Object(void)
{}

Object::Object(std::string name) :
  m_name(name)
{}

Object::Object(Object const & object) :
  m_name(object.m_name)
{}

Object::~Object(void)
{}

bool
Object::operator == (Object const & O) const
{
  return get_name() == O.get_name();
}

std::ostream & 
Object::printToStream(std::ostream & flux) const
{ 
  flux  << " Name: " << get_name();

  return flux;
}

std::ostream &
operator << (std::ostream & flux, Object const & object)
{
  return object.printToStream(flux);
}
