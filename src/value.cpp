#include "value.h"

namespace hkl {

  Value::Value(void)
    : Object()
    {}

  Value::Value(std::string const & name, double value)
    : Object(name),
    m_value(value)
    {}

  Value::Value(Value const & value)
    : Object(value.get_name()),
    m_value(value.m_value)
    {}

  Value::~Value(void)
  {}


  bool
  Value::operator == (Value const & value) const
  {
    return Object::operator==(value)
      && fabs(m_value - value.m_value) < constant::math::epsilon_1;
  }

  Value &
  Value::operator += (Value const & value)
  {
    m_value += value.m_value;
    return *this;
  }

  Value &
  Value::operator -= (Value const & value)
  {
    m_value -= value.m_value;
    return *this;
  }

  Value &
  Value::operator /= (double const & d)
  {
    m_value /= d;
    return *this;
  }

  Value &
  Value::operator *= (double const & d)
  {
    m_value *= d;
    return *this;
  }

  std::ostream & 
  Value::printToStream(std::ostream & flux) const
  {  
    Object::printToStream(flux);

    flux  << " Value: " << m_value << std::endl;

    return flux;
  }

} // namespace hkl

std::ostream &
operator <<(std::ostream & flux, hkl::Value const & value)
{
  return value.printToStream(flux);
}
