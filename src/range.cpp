
#include "range.h"

namespace hkl {

/**
 * @brief The default constructor.
 */
Range::Range() :
  _min(Value()),
  _current(Value()),
  _max(Value()) 
{
  // Bouml preserved body begin 00024802
  // Bouml preserved body end 00024802
}

/**
 * @brief constructor of the Range class.
 * 
 * @param min The minimum value of the Range.
 * @param current The current value of the Range.
 * @param max The maximum value of the Range.
 * @throw HKLException if not min < current < max; 
 */
Range::Range(const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException) :
  _min(min),
  _current(current),
  _max(max)
{
  // Bouml preserved body begin 00024A02
      if (min <= current && current <= max)
        {
          _min = min;
          _current = current;
          _max = max;
        }
      else
        {
          ostringstream reason;
          reason << "Can not create such a range " << min << " <= " << current << " <= " << max << endl;
          HKLEXCEPTION(reason.str(), "set a valid range");
        }
  // Bouml preserved body end 00024A02
}

Range::Range(const hkl::Range & source) :
  _min(source._min),
  _current(source._current),
  _max(source._max) 
{
  // Bouml preserved body begin 00024902
  // Bouml preserved body end 00024902
}

/**
 * @brief Set the _current hkl::Value of the Range class.
 * @param current The hkl::Value to set.
 * @throw An HKLException if the current hkl::Value in not between min and max.
 */
void Range::set_current(const hkl::Value & current) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00024B02
      if (_min <= (current + hkl::constant::math::epsilon) && (current - hkl::constant::math::epsilon) <= _max)
          _current = current;
      else
        {
          std::ostringstream reason;
          reason << "Can not set this current value : " << current.get_value()
          << " outside (" << _min.get_value() << ":" << _max.get_value() << ")";
          HKLEXCEPTION(reason.str(), "Change the current value or the minimun and maximum range.");
        }
  // Bouml preserved body end 00024B02
}

/**
 * @brief Set the _current double of the Range class.
 * @param current The double to set.
 *
 * This method do not check for the validity of the Range. This method
 * is requiered by the simplex affinement.
 */
void Range::set_current(const double & current) 
{
  // Bouml preserved body begin 00024D02
      _current.set_value(current);
  // Bouml preserved body end 00024D02
}

/**
 * @brief Set the minimum and the maximum of the Range class.
 * @param min The minimum hkl::Value to set.
 * @param max The maximum hkl::Value to set.
 * @throw HKLException if the new Range is not valid.
 *
 * this method check that the new minimun is not bigger than the current
 * value of the Range and greater than the maximum.
 */
void Range::set_range(const hkl::Value & min, const hkl::Value & max) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00024D82
      if (min <= _current + hkl::constant::math::epsilon)
        _min = min;
      else
        {
          std::ostringstream reason;
          reason << "Can not set a minimum (" << min << ") greater than the current value (" << _current << ")";
          HKLEXCEPTION(reason.str(), "Change the current value or the minimun range.");
        }
      
      if (_current - hkl::constant::math::epsilon <= max)
        _max = max;
      else
        {
          std::ostringstream reason;
          reason << "Can not set a maximum (" << max << ") lower than the current value (" << _current << ")";
          HKLEXCEPTION(reason.str(), "Change the current value or the minimun range.");
        }
  // Bouml preserved body end 00024D82
}

/**
 * @brief Set the minimum and the maximum of the Range class.
 * @param min The minimum double to set.
 * @param current The current double to set.
 * @param max The maximum double to set.
 * @throw HKLException if the new Range is not valid.
 *
 * this method do not check that the new minimun is not bigger than the current
 * value of the range and greater than the maximum.
 */
void Range::set(double min, double current, double max) 
{
  // Bouml preserved body begin 00024E02
      _min = min;
      _current = current;
      _max = max;
  // Bouml preserved body end 00024E02
}

/**
 * @brief Set a Range from another one.
 * @param range The Range to set.
 *
 * this method set only the _min, _current, _max Value of the Range.
 */
void Range::set(const Range & range) 
{
  // Bouml preserved body begin 00024E82
      _min = range._min;
      _current = range._current;
      _max = range._max;
  // Bouml preserved body end 00024E82
}

/**
 * @brief Multiply a Range by another one.
 * @param range The Range to multiply by.
 * @return A Range ref on the Range after the multiplication.
 *
 * This method modify min, current and max to reflect the multiplication.
 */

Range & Range::operator*=(const Range & range) 
{
  // Bouml preserved body begin 00024F02
      double m1 = _min.get_value() * range._min.get_value();
      double m2 = _min.get_value() * range._max.get_value();
      double m3 = _max.get_value() * range._min.get_value();
      double m4 = _max.get_value() * range._max.get_value();
      
      double min = m1;
      if (m2 < min)
        min = m2;
      if (m3 < min)
        min = m3;
      if (m4 < min)
        min = m4;
      
      double max = m1;
      if (m2 > max)
        max = m2;
      if (m3 > max)
        max = m3;
      if (m4 > max)
        max = m4;
      
      _min = min;
      _current *= range._current;
      _max = max;
      
      return *this;
  // Bouml preserved body end 00024F02
}

/**
 * @brief Multiply a Range by a double value.
 * @param d The double value.
 * @return The Range after the multiplication.
 *
 * This method modify min, current and max to reflect the multiplication.
 */

Range & Range::operator*=(const double & d) 
{
  // Bouml preserved body begin 00024F82
      double min;
      double max;
      if (d < 0)
        {
          min = _max.get_value() * d;
          max = _min.get_value() * d;
        }
      else
        {
          min = _min.get_value() * d;
          max = _max.get_value() * d;
        }
      _min = min;
      _current *= d;
      _max = max;
      
      return *this;
  // Bouml preserved body end 00024F82
}

/**
 * @brief check if the Range contain zero.
 * @return true if zero is include in between min, max.
 */

bool Range::contain_zero() const 
{
  // Bouml preserved body begin 00025002
      if (_min <= 0 && _max >= 0)
        return true;
      else
        return false;
  // Bouml preserved body end 00025002
}

/*!
 * \brief Are two Range equals ?
 * \param range the Range to compare with.
 */

bool Range::operator==(const Range & range) const 
{
  // Bouml preserved body begin 00025082
      return _current == range._current
        && _min == range._min
        && _max == range._max;
  // Bouml preserved body end 00025082
}

/*!
 * \brief print the Range into a flux
 * \param flux The stream to print into.
 */
ostream & Range::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00025102
      flux
      << _current.get_value()
      << " [ " << _min.get_value()
      << " : "
      << _max.get_value() << " ]" << endl;
      
      return flux;
  // Bouml preserved body end 00025102
}

/*!
 * \brief Save the Range into a stream.
 * \param flux the stream to save the Range into.
 * \return The stream with the Range.
 */
ostream & Range::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00025182
      _current.toStream(flux);
      _min.toStream(flux);
      _max.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00025182
}

/*!
 * \brief Restore a Range from a stream.
 * \param flux The stream containing the Range to restore.
 * @todo call update_observers or not ?
 */
istream & Range::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00025202
      _current.fromStream(flux);
      _min.fromStream(flux);
      _max.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00025202
}


} // namespace hkl
