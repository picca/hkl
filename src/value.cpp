/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
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
      return fabs(_value - value._value) < constant::math::epsilon;
#else
      if (::isinf(_value) && ::isinf(value._value) && !(::isinf(_value) - ::isinf(value._value)))
        return true;
      else
        return fabs(_value - value._value) < constant::math::epsilon;
#endif
    }

  bool Value::operator!=(const hkl::Value & value) const
    {
      return fabs(_value - value._value) > constant::math::epsilon;
    }

  bool Value::operator<=(const hkl::Value & value) const
    {
      return _value <= value._value + constant::math::epsilon;
    }

  bool Value::operator>=(const hkl::Value & value) const
    {
      return _value >= value._value - constant::math::epsilon;
    }

  bool Value::operator<(const hkl::Value & value) const
    {
      return _value < value._value + constant::math::epsilon;
    }

  bool Value::operator>(const hkl::Value & value) const
    {
      return _value > value._value - constant::math::epsilon;
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

  /*!
   * \brief Save the Value into a stream.
   * \param flux the stream to save the Value into.
   * \return The stream with the Value.
   */
  std::ostream & Value::toStream(std::ostream & flux) const
    {
      flux << std::setprecision(constant::math::precision) << " " << _value;

      return flux;
    }

  /*!
   * \brief Restore a Value from a stream.
   * \param flux The stream containing the Value to restore.
   */
  std::istream & Value::fromStream(std::istream & flux)
  {
    flux >> std::setprecision(constant::math::precision) >> _value;

    return flux;
  }


} // namespace hkl
