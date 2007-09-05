
#include "value.h"

namespace hkl {

/**
 * @brief The default constructor.
 */
Value::Value() :
  _value(0)
{
  // Bouml preserved body begin 0001F4BC
      
      
  // Bouml preserved body end 0001F4BC
}

/**
 * @brief A constructor from a double
 */
Value::Value(const double & value) :
  _value(value)
{
  // Bouml preserved body begin 0001F436
  // Bouml preserved body end 0001F436
}

Value::Value(const hkl::Value & source) :
 _value(source._value) 
{
  // Bouml preserved body begin 00024202
  // Bouml preserved body end 00024202
}

/**
 *  @brief The set accessor
 *  @param value the Value to set.
 */
void Value::set_value(const double & value) 
{
  _value = value;
}

bool Value::operator==(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F56B
  if (::isinf(_value) && ::isinf(value._value) && !(::isinf(_value) - ::isinf(value._value)))
    return true;
  else
    return fabs(_value - value._value) < constant::math::epsilon;
  // Bouml preserved body end 0001F56B
}

bool Value::operator!=(const hkl::Value & value) const 
{
  // Bouml preserved body begin 00040502
    return fabs(_value - value._value) > constant::math::epsilon;
  // Bouml preserved body end 00040502
}

bool Value::operator<=(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F404
    return _value <= value._value + constant::math::epsilon;
  // Bouml preserved body end 0001F404
}

bool Value::operator>=(const hkl::Value & value) const 
{
  // Bouml preserved body begin 00025282
    return _value >= value._value - constant::math::epsilon;
  // Bouml preserved body end 00025282
}

bool Value::operator<(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F504
      return _value < value._value + constant::math::epsilon;
  // Bouml preserved body end 0001F504
}

bool Value::operator>(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F584
      return _value > value._value - constant::math::epsilon;
  // Bouml preserved body end 0001F584
}

hkl::Value & Value::operator+=(const hkl::Value & value) 
{
  // Bouml preserved body begin 0001F604
      _value += value._value;
      return *this;
  // Bouml preserved body end 0001F604
}

hkl::Value & Value::operator-=(const hkl::Value & value) 
{
  // Bouml preserved body begin 0001F684
      _value -= value._value;
      return *this;
  // Bouml preserved body end 0001F684
}

hkl::Value & Value::operator*=(const hkl::Value & value) 
{
  // Bouml preserved body begin 0001F704
      _value *= value._value;
      return *this;
  // Bouml preserved body end 0001F704
}

hkl::Value & Value::operator/=(const hkl::Value & value) 
{
  // Bouml preserved body begin 0001F784
      _value /= value._value;
      return *this;
  // Bouml preserved body end 0001F784
}

hkl::Value Value::operator+(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F804
      Value res(*this);
      res += value;
      
      return res;
  // Bouml preserved body end 0001F804
}

hkl::Value Value::operator-(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F884
      Value res(*this);
      res -= value;
      
      return res;
  // Bouml preserved body end 0001F884
}

hkl::Value Value::operator*(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F904
      Value res(*this);
      res *= value;
      
      return res;
  // Bouml preserved body end 0001F904
}

hkl::Value Value::operator/(const hkl::Value & value) const 
{
  // Bouml preserved body begin 0001F984
      Value res(*this);
      res /= value;
      
      return res;
  // Bouml preserved body end 0001F984
}

/*!
 * \brief print the Value into a flux
 * \param flux The stream to print into.
 */
std::ostream & Value::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0001FA04
      flux << _value ;
      return flux;
  // Bouml preserved body end 0001FA04
}

/*!
 * \brief Save the Value into a stream.
 * \param flux the stream to save the Value into.
 * \return The stream with the Value.
 */
std::ostream & Value::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0001F4B6
      flux << std::setprecision(constant::math::precision) << " " << _value;
      
      return flux;
  // Bouml preserved body end 0001F4B6
}

/*!
 * \brief Restore a Value from a stream.
 * \param flux The stream containing the Value to restore.
 */
std::istream & Value::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 0001F536
      flux >> std::setprecision(constant::math::precision) >> _value;
      
      return flux;
  // Bouml preserved body end 0001F536
}


} // namespace hkl
