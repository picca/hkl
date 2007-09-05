
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
