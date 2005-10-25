#include "value.h"

namespace hkl {

  Value::Value(void)
    : Object()
    {}

  Value::Value(std::string const & name, double value)
    : Object(name)
    {
      set_value(value);
    }

  Value::Value(Value const & value)
    : Object(value.get_name())
    {
      set_value(value.get_value());
    }

  Value::~Value(void)
  {}


  bool
    Value::operator == (Value const & value) const
    {
      return Object::operator==(value)
        && fabs(get_value() - value.get_value()) < constant::math::epsilon_1;
    }

  Value &
    Value::operator += (Value const & value)
    {
      set_value(get_value() + value.get_value());
      return *this;
    }

  Value &
    Value::operator -= (Value const & value)
    {
      set_value(get_value() - value.get_value());
      return *this;
    }

  Value &
    Value::operator /= (double const & d)
    {
      set_value(get_value() / d);
      return *this;
    }

  Value &
    Value::operator *= (double const & d)
    {
      set_value(get_value() * d);
      return *this;
    }

  std::ostream & 
    Value::printToStream(std::ostream & flux) const
    {  
      Object::printToStream(flux);

      flux  << " Value: " << get_value();

      return flux;
    }

} // namespace hkl

std::ostream &
operator <<(std::ostream & flux, hkl::Value const & value)
{
  return value.printToStream(flux);
}
