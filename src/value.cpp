#include <iomanip>

#include "value.h"
#include "constants.h"

namespace hkl
  {

  Value::Value(void) :
      _value(0.)
  {}

  Value::Value(double const & value) :
      _value(value)
  {}

  bool
  Value::operator == (Value const & value) const
    {
      return fabs(_value - value._value) < constant::math::epsilon2;
    }

  bool
  Value::operator <= (Value const & value) const
    {
      return _value <= value._value;
    }

  bool
  Value::operator < (Value const & value) const
    {
      return _value < value._value;
    }

  bool
  Value::operator > (Value const & value) const
    {
      return _value > value._value;
    }

  Value &
  Value::operator += (Value const & value)
  {
    _value += value._value;

    return *this;
  }

  Value &
  Value::operator -= (Value const & value)
  {
    _value -= value._value;
    return *this;
  }

  Value &
  Value::operator *= (Value const & value)
  {
    _value *= value._value;
    return *this;
  }

  Value &
  Value::operator /= (Value const & value)
  {
    _value /= value._value;
    return *this;
  }

  Value
  Value::operator + (Value const & value) const
    {
      Value res(*this);
      res += value;

      return res;
    }

  Value
  Value::operator - (Value const & value) const
    {
      Value res(*this);
      res -= value;

      return res;
    }

  Value
  Value::operator * (Value const & value) const
    {
      Value res(*this);
      res *= value;

      return res;
    }

  Value
  Value::operator / (Value const & value) const
    {
      Value res(*this);
      res /= value;

      return res;
    }

  ostream &
  Value::printToStream(ostream & flux) const
    {
      flux << " Value : " << _value ;

      return flux;
    }

  ostream &
  Value::toStream(ostream & flux) const
    {
      flux << setprecision(constant::math::precision) << " " << _value << endl;

      return flux;
    }

  istream &
  Value::fromStream(istream & flux)
  {
    flux >> setprecision(constant::math::precision) >> _value;

    return flux;
  }

} // namespace hkl
