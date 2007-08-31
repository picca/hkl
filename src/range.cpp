
#include <cmath>
#include "range.h"

namespace hkl {

/**
 * @brief The default constructor.
 */
Range::Range() :
  _min(Value()),
  _current(Value()),
  _consign(Value()),
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
 * @param consign The consign value of the Range.
 * @param max The maximum value of the Range.
 * @throw HKLException if not min < current, consign < max; 
 */
Range::Range(const hkl::Value & min, const hkl::Value & current, const hkl::Value & consign, const hkl::Value & max) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00024A02
    if (min <= current && current <= max && min <= consign && consign <= max)
      {
        _min = min;
        _current = current;
        _consign = consign;
        _max = max;
      }
    else
      {
        std::ostringstream reason;
        reason << "Can not create such a range " << min << " <= [" << current << ", " << consign << "] <= " << max << std::endl;
        HKLEXCEPTION(reason.str(), "set a valid range");
      }
  // Bouml preserved body end 00024A02
}

Range::Range(const hkl::Range & source) :
  _min(source._min),
  _current(source._current),
  _consign(source._consign),
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
    if (_min <= current && current <= _max)
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
void Range::set_current(double current) 
{
  // Bouml preserved body begin 00024D02
    _current.set_value(current);
  // Bouml preserved body end 00024D02
}

/**
 * @brief Set the consign hkl::Value of the Range class.
 * @param consign The hkl::Value to set.
 * @throw An HKLException if the consign hkl::Value in not in between min and max.
 */
void Range::set_consign(const hkl::Value & consign) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003F382
        if (_min <= consign && consign <= _max)
            _consign = consign;
        else
          {
            std::ostringstream reason;
            reason << "Can not set this consign value : " << consign.get_value()
            << " outside (" << _min.get_value() << ":" << _max.get_value() << ")";
            HKLEXCEPTION(reason.str(), "Change the consign value or the minimun and maximum range.");
          }
  // Bouml preserved body end 0003F382
}

/**
 * @brief Set the minimum and the maximum of the Range class.
 * @param min The minimum hkl::Value to set.
 * @param max The maximum hkl::Value to set.
 * @throw HKLException if the new Range is not valid.
 * @todo maybe split in set_min and set_max
 * 
 * this method check that the new minimun is not bigger than the current or the consign
 * value of the Range and than the maximum is not lower than the current or consign.
 */
void Range::set_range(const hkl::Value & min, const hkl::Value & max) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00024D82
    if (min <= _current && min <= _consign)
      {
        if (_current <= max && _consign <= max)
        {
            _min = min;
            _max = max;
        }
        else
          {
            std::ostringstream reason;
            reason << "Can not set a maximum (" << max << ") lower than the current value (" << _current << ", " << _consign << ")";
            HKLEXCEPTION(reason.str(), "Change the current value or the minimun range.");
          }
      }
    else
      {
        std::ostringstream reason;
        reason << "Can not set a minimum (" << min << ") greater than the current/consign value (" << _current << ", " << _consign << ")";
        HKLEXCEPTION(reason.str(), "Change the current/consign value before setting the minimun range.");
      }
  // Bouml preserved body end 00024D82
}

/**
 * @brief Set the minimum and the maximum of the Range class.
 * @param min The minimum double to set.
 * @param current The current double to set.
 * @param consign The consign double to set.
 * @param max The maximum double to set.
 * @throw HKLException if the new Range is not valid.
 * 
 * this method do not check that the new minimun is not bigger than the current
 * value of the range and greater than the maximum.
 */
void Range::set(double min, double current, double consign, double max) 
{
  // Bouml preserved body begin 00024E02
      _min = min;
      _current = current;
      _consign = consign;
      _max = max;
  // Bouml preserved body end 00024E02
}

/**
 * @brief Set a Range from another one.
 * @param range The hkl::Range to set.
 * 
 * this method set only the _min, _current, _consign and _max Value of the Range.
 */
void Range::set(const hkl::Range & range) 
{
  // Bouml preserved body begin 00024E82
      _min = range._min;
      _current = range._current;
      _consign = range._consign;
      _max = range._max;
  // Bouml preserved body end 00024E82
}

/**
 * @brief Add a Range to another one.
 * @param range The Range to add.
 * @return A Range ref on the Range after the addition.
 * 
 * This method modify min, current and max to reflect the addition.
 */
hkl::Range & Range::operator+=(const hkl::Range & range) 
{
  // Bouml preserved body begin 00041E02
    _min += range._min;
    _current += range._current;
    _consign += range._consign;
    _max += range._max;

    return *this;
  // Bouml preserved body end 00041E02
}

/**
 * @brief Add a Range to another one.
 * @param value The Range to add.
 * @return A Range ref on the Range after the addition.
 * 
 * This method modify min, current and max to reflect the addition.
 */
hkl::Range & Range::operator+=(const double & value) throw(hkl::Affinement) 
{
  // Bouml preserved body begin 00041E82
    _min += value;
    _current += value;
    _consign += value;
    _max += value;

    return *this;
  // Bouml preserved body end 00041E82
}

/**
 * @brief Multiply a Range by another one.
 * @param range The Range to multiply by.
 * @return A Range ref on the Range after the multiplication.
 * 
 * This method modify min, current and max to reflect the multiplication.
 */
hkl::Range & Range::operator*=(const hkl::Range & range) 
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
      _consign *= range._consign;
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
hkl::Range & Range::operator*=(double d) 
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
      _consign *= d;
      _max = max;
      
      return *this;
  // Bouml preserved body end 00024F82
}

/**
 * @brief Divide a Range by a double value.
 * @param d The double value.
 * @return The Range divided.
 * 
 * This method modify min, current, consign and max.
 */
hkl::Range & Range::operator/=(const double & d) 
{
  // Bouml preserved body begin 00041982
    double min = _min.get_value() / d;
    double max = _max.get_value() / d;
    if (min > max)
      {
        double tmp = min;
        min = max;
        max = tmp;
      }
    _min.set_value(min);
    _current /= d;
    _consign /= d;
    _max.set_value(max);

    return *this;
  // Bouml preserved body end 00041982
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

/**
 * @brief compute the cos of the range.
 * @return The cosinus of the Range
 */
hkl::Range & Range::cos() 
{
  // Bouml preserved body begin 00041B02
  double const & min = _min.get_value();
  double const & current = _current.get_value();
  double const & consign = _consign.get_value();
  double const & max = _max.get_value();

  if (max - min >= 2 * hkl::constant::math::pi)
  {
    this->set(-1, ::cos(current), ::cos(consign), 1);
  }
  else
    {
      int quad_min = (int)floor(2 * min / hkl::constant::math::pi) % 4;
      if (quad_min < 0)
        quad_min += 4;

      int quad_max = (int)floor(2 * max / hkl::constant::math::pi) % 4;
      if (quad_max < 0)
        quad_max += 4;

      switch (quad_max)
        {
        case 0:
          switch (quad_min)
            {
            case 0:
              this->set(::cos(max), ::cos(current), ::cos(consign), ::cos(min));
              break;
            case 1:
              this->set(-1, ::cos(current), ::cos(consign), 1);
              break;
            case 2:
              this->set(::cos(min), ::cos(current), ::cos(consign), 1);
              break;
            case 3:
              if (::cos(min) < ::cos(max))
                this->set(::cos(min), ::cos(current), ::cos(consign), 1);
              else
                this->set(::cos(max), ::cos(current), ::cos(consign), 1);
              break;
            }
          break;
        case 1:
          switch (quad_min)
            {
            case 0:
              this->set(::cos(max), ::cos(current), ::cos(consign), ::cos(min));
              break;
            case 1:
              this->set(-1, ::cos(current), ::cos(consign), 1);
              break;
            case 2:
              if (::cos(min) < ::cos(max))
                this->set(::cos(min), ::cos(current), ::cos(consign ), 1);
              else
                this->set(::cos(max), ::cos(current), ::cos(consign), 1);
              break;
            case 3:
              this->set(::cos(max), ::cos(current), ::cos(consign), 1);
              break;
            }
          break;
        case 2:
          switch (quad_min)
            {
            case 0:
              this->set(-1, ::cos(current), ::cos(consign), ::cos(min));
              break;
            case 1:
              if (::cos(min) < ::cos(max))
                this->set(-1, ::cos(current), ::cos(consign), ::cos(max));
              else
                this->set(-1, ::cos(current), ::cos(consign), ::cos(min));
              break;
            case 2:
              if (::cos(min) < ::cos(max))
                this->set(::cos(min), ::cos(current), ::cos(consign), ::cos(max));
              else
                this->set(-1, ::cos(current), ::cos(consign), 1);
              break;
            case 3:
              this->set(-1, ::cos(current), ::cos(consign), 1);
              break;
            }
          break;
        case 3:
          switch (quad_min)
            {
            case 0:
              if (::cos(min) < ::cos(max))
                this->set(-1, ::cos(current), ::cos(consign), ::cos(max));
              else
                this->set(-1, ::cos(current), ::cos(consign), ::cos(min));
              break;
            case 1:
              this->set(-1, ::cos(current), ::cos(consign), ::cos(max));
              break;
            case 2:
              this->set(::cos(min), ::cos(current), ::cos(consign), ::cos(max));
              break;
            case 3:
              if (::cos(min) < ::cos(max))
                this->set(::cos(min), ::cos(current), ::cos(consign), ::cos(max));
              else
                this->set(-1, ::cos(current), ::cos(consign), 1);
              break;
            }
          break;
        }
    }
  return *this;
  // Bouml preserved body end 00041B02
}

/**
 * @brief compute the acos of the range.
 * @return The invert cosinus of the Range
 */
hkl::Range & Range::acos() 
{
  // Bouml preserved body begin 00041B82
  double min = ::acos(_max.get_value());
  double current = ::acos(_current.get_value());
  double consign = ::acos(_consign.get_value());
  double max = ::acos(_min.get_value());

  this->set(min, current, consign, max);
  return *this;
  // Bouml preserved body end 00041B82
}

/**
 * @brief compute the sinus of the range.
 * @return the sinus of the Range
 */
hkl::Range & Range::sin() 
{
  // Bouml preserved body begin 00041C02
    double min = _min.get_value();
    double current = _current.get_value();
    double consign = _consign.get_value();
    double max = _max.get_value();

    /* if there is at least one period in b, then a = [-1, 1] */
    if ( max - min >= 2 * hkl::constant::math::pi)
        this->set(-1, ::sin(current), ::sin(consign), 1);
    else
      {
        int quad_min = (int)floor(2 * min / hkl::constant::math::pi) % 4;
        if (quad_min < 0)
            quad_min += 4;

        int quad_max = (int)floor(2 * max / hkl::constant::math::pi) % 4;
        if (quad_max < 0)
            quad_max += 4;

        switch (quad_max) {
          case 0:
            switch (quad_min) {
              case 0:
                if (::sin(min) < ::sin(max))
                    this->set(::sin(min), ::sin(current), ::sin(consign), ::sin(max));
                else
                    this->set(-1, ::sin(current), ::sin(consign), 1);
                break;
              case 3:
                this->set(::sin(min), ::sin(current), ::sin(consign), ::sin(max));
                break;
              case 1:
                if (::sin(min) > ::sin(max))
                    this->set(-1, ::sin(current), ::sin(consign), ::sin(min));
                else
                    this->set(-1, ::sin(current), ::sin(consign), ::sin(max));
                break;
              case 2:
                this->set(-1, ::sin(current), ::sin(consign), ::sin(max));
                break;
            }
            break;
          case 1:
            switch (quad_min) {
              case 0:
                if (::sin(min) < ::sin(max))
                    this->set(::sin(min), ::sin(current), ::sin(consign), 1);
                else
                    this->set(::sin(max), ::sin(current), ::sin(consign), 1);
                break;
              case 1:
                if (::sin(min) < ::sin(max))
                    this->set(-1, ::sin(current), ::sin(consign), 1);
                else
                    this->set(::sin(max), ::sin(current), ::sin(consign), ::sin(min));
                break;
              case 2:
                this->set(-1, ::sin(current), ::sin(consign), 1);
                break;
              case 3:
                this->set(::sin(min), ::sin(current), ::sin(consign), 1);
                break;
            }
            break;
          case 2:
            switch (quad_min) {
              case 0:
                this->set(::sin(max), ::sin(current), ::sin(consign), 1);
                break;
              case 1:
              case 2:
                if (::sin(min) < ::sin(max))
                    this->set(-1, ::sin(current), ::sin(consign), 1);
                else
                    this->set(::sin(max), ::sin(current), ::sin(consign), ::sin(min));
                break;
              case 3:
                if (::sin(min) < ::sin(max))
                    this->set(::sin(min), ::sin(current), ::sin(consign), 1);
                else
                    this->set(::sin(max), ::sin(current), ::sin(consign), 1);
                break;
            }
            break;
          case 3:
            switch (quad_min) {
              case 0:
                this->set(-1, ::sin(current), ::sin(consign), 1);
                break;
              case 1:
                this->set(-1, ::sin(current), ::sin(consign), ::sin(min));
                break;
              case 2:
                if(::sin(min) < ::sin(max))
                    this->set(-1, ::sin(current), ::sin(consign), ::sin(max));
                else
                    this->set(-1, ::sin(current), ::sin(consign), ::sin(min));
                break;
              case 3:
                if (::sin(min) < ::sin(max))
                    this->set(::sin(min), ::sin(current), ::sin(consign), ::sin(max));
                else
                    this->set(-1, ::sin(current), ::sin(consign), 1);
                break;
            }
            break;
        }
      }
  return *this;
  // Bouml preserved body end 00041C02
}

/**
 * @brief compute the invert sinus of the range.
 * @return the invert sinus of the Range
 */
hkl::Range & Range::asin() 
{
  // Bouml preserved body begin 00041C82
  double min = ::asin(_min.get_value());
  double current = ::asin(_current.get_value());
  double consign = ::asin(_consign.get_value());
  double max = ::asin(_max.get_value());

  this->set(min, current, consign, max);
  return *this;
  // Bouml preserved body end 00041C82
}

/**
 * @brief compute the tangente of the range.
 * @todo test
 */
hkl::Range & Range::tan() 
{
  // Bouml preserved body begin 00041D02
  double const & min = _min.get_value();
  double const & current = _current.get_value();
  double const & consign = _consign.get_value();
  double const & max = _max.get_value();
  
  int quadrant_down = (int)floor( 2 * min / hkl::constant::math::pi);
  int quadrant_up = (int)floor(2 * max / hkl::constant::math::pi);

  /* if there is at least one period in b or if b contains a Pi/2 + k*Pi, */
  /* then a = ]-oo, +oo[ */
std::cout << "min : " << min << "(" << quadrant_down << ") max : " << max << "(" << quadrant_up << ")" << std::endl;
  if ( ((quadrant_up - quadrant_down) >= 2)
       || ((quadrant_down % 2) && !(quadrant_up % 2)) )
      this->set(-hkl::constant::math::infinity, ::tan(current), ::tan(consign), hkl::constant::math::infinity);
  else
      this->set(::tan(min), ::tan(current), ::tan(consign), ::tan(max));
  return *this;
  // Bouml preserved body end 00041D02
}

/**
 * @brief compute the invert tangente of the range.
 * @todo test
 */
hkl::Range & Range::atan() 
{
  // Bouml preserved body begin 00041D82
  double min = ::atan(_min.get_value());
  double current = ::atan(_current.get_value());
  double consign = ::atan(_consign.get_value());
  double max = ::atan(_max.get_value());
  
  this->set(min, current, consign, max);
  return *this;
  // Bouml preserved body end 00041D82
}

/*!
 * \brief Are two Range equals ?
 * \param range the hkl::Range to compare with.
 */

bool Range::operator==(const hkl::Range & range) const 
{
  // Bouml preserved body begin 00025082
    return _current == range._current
           && _consign == range._consign
           && _min == range._min
           && _max == range._max;
  // Bouml preserved body end 00025082
}

/*!
 * \brief print the Range into a flux
 * \param flux The stream to print into.
 */
std::ostream & Range::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00025102
      flux
      << _current.get_value()
      << " " << _consign.get_value()
      << " [ " << _min.get_value()
      << " : "
      << _max.get_value() << " ]";
      
      return flux;
  // Bouml preserved body end 00025102
}

/*!
 * \brief Save the Range into a stream.
 * \param flux the stream to save the Range into.
 * \return The stream with the Range.
 */
std::ostream & Range::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00025182
      _min.toStream(flux);
      _current.toStream(flux);
      _consign.toStream(flux);
      _max.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00025182
}

/*!
 * \brief Restore a Range from a stream.
 * \param flux The stream containing the Range to restore.
 * @todo call update_observers or not ?
 */
std::istream & Range::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00025202
      _min.fromStream(flux);
      _current.fromStream(flux);
      _consign.fromStream(flux);
      _max.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 00025202
}


} // namespace hkl
