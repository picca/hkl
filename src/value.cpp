#include "config.h"
#include "value.h"

namespace hkl
  {

  /**
   * @brief The default constructor.
   */
  Value::Value() :
      _value(0)
  {


  }

  /**
   * @brief A constructor from a double
   */
  Value::Value(const double & value) :
      _value(value)
  {
  }

  Value::Value(const hkl::Value & source) :
      _value(source._value)
  {
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
#if _MSC_VER && _MSC_VER <= 1200
      return fabs(_value - value._value) < HKL_EPSILON;
#else
      if (::isinf(_value) && ::isinf(value._value) && !(::isinf(_value) - ::isinf(value._value)))
        return true;
      else
        return fabs(_value - value._value) < HKL_EPSILON;
#endif
    }

  bool Value::operator!=(const hkl::Value & value) const
    {
      return fabs(_value - value._value) > HKL_EPSILON;
    }

  bool Value::operator<=(const hkl::Value & value) const
    {
      return _value <= value._value + HKL_EPSILON;
    }

  bool Value::operator>=(const hkl::Value & value) const
    {
      return _value >= value._value - HKL_EPSILON;
    }

  bool Value::operator<(const hkl::Value & value) const
    {
      return _value < value._value + HKL_EPSILON;
    }

  bool Value::operator>(const hkl::Value & value) const
    {
      return _value > value._value - HKL_EPSILON;
    }

  hkl::Value & Value::operator+=(const hkl::Value & value)
  {
    _value += value._value;
    return *this;
  }

  hkl::Value & Value::operator-=(const hkl::Value & value)
  {
    _value -= value._value;
    return *this;
  }

  hkl::Value & Value::operator*=(const hkl::Value & value)
  {
    _value *= value._value;
    return *this;
  }

  hkl::Value & Value::operator/=(const hkl::Value & value)
  {
    _value /= value._value;
    return *this;
  }

  hkl::Value Value::operator+(const hkl::Value & value) const
    {
      Value res(*this);
      res += value;

      return res;
    }

  hkl::Value Value::operator-(const hkl::Value & value) const
    {
      Value res(*this);
      res -= value;

      return res;
    }

  hkl::Value Value::operator*(const hkl::Value & value) const
    {
      Value res(*this);
      res *= value;

      return res;
    }

  hkl::Value Value::operator/(const hkl::Value & value) const
    {
      Value res(*this);
      res /= value;

      return res;
    }

  /*!
   * \brief print the Value into a flux
   * \param flux The stream to print into.
   */
  std::ostream & Value::printToStream(std::ostream & flux) const
    {
      flux << _value ;
      return flux;
    }

} // namespace hkl
