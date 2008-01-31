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

#include <cmath>
#include "range.h"

namespace hkl
  {

  /**
   * @brief The default constructor.
   */
  Range::Range() :
      _min(Value()),
      _current(Value()),
      _consign(Value()),
      _max(Value())
  {
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
  }

  Range::Range(const hkl::Range & source) :
      _min(source._min),
      _current(source._current),
      _consign(source._consign),
      _max(source._max)
  {
  }

  /**
   * @brief Set the _current hkl::Value of the Range class.
   * @param current The hkl::Value to set.
   * @throw An HKLException if the current hkl::Value in not between min and max.
   */
  void Range::set_current(const hkl::Value & current) throw(hkl::HKLException)
  {
    if (_min <= current && current <= _max)
      _current = current;
    else
      {
        std::ostringstream reason;
        reason << "Can not set this current value : " << current.get_value()
        << " outside (" << _min.get_value() << ":" << _max.get_value() << ")";
        HKLEXCEPTION(reason.str(), "Change the current value or the minimun and maximum range.");
      }
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
    _current.set_value(current);
  }

  /**
   * @brief Set the consign hkl::Value of the Range class.
   * @param consign The hkl::Value to set.
   * @throw An HKLException if the consign hkl::Value in not in between min and max.
   */
  void Range::set_consign(const hkl::Value & consign) throw(hkl::HKLException)
  {
    if (_min <= consign && consign <= _max)
      _consign = consign;
    else
      {
        std::ostringstream reason;
        reason << "Can not set this consign value : " << consign.get_value()
        << " outside (" << _min.get_value() << ":" << _max.get_value() << ")";
        HKLEXCEPTION(reason.str(), "Change the consign value or the minimun and maximum range.");
      }
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
    _min = min;
    _current = current;
    _consign = consign;
    _max = max;
  }

  /**
   * @brief Set a Range from another one.
   * @param range The hkl::Range to set.
   *
   * this method set only the _min, _current, _consign and _max Value of the Range.
   */
  void Range::set(const hkl::Range & range)
  {
    _min = range._min;
    _current = range._current;
    _consign = range._consign;
    _max = range._max;
  }

  /*!
   * \brief Are two Range equals ?
   * \param range the hkl::Range to compare with.
   */

  bool Range::operator==(const hkl::Range & range) const
    {
      return _current == range._current
             && _consign == range._consign
             && _min == range._min
             && _max == range._max;
    }

  /*!
   * \brief print the Range into a flux
   * \param flux The stream to print into.
   */
  std::ostream & Range::printToStream(std::ostream & flux) const
    {
      flux
      << _current.get_value()
      << " " << _consign.get_value()
      << " [ " << _min.get_value()
      << " : "
      << _max.get_value() << " ]";

      return flux;
    }

  /*!
   * \brief Save the Range into a stream.
   * \param flux the stream to save the Range into.
   * \return The stream with the Range.
   */
  std::ostream & Range::toStream(std::ostream & flux) const
    {
      _min.toStream(flux);
      _current.toStream(flux);
      _consign.toStream(flux);
      _max.toStream(flux);

      return flux;
    }

  /*!
   * \brief Restore a Range from a stream.
   * \param flux The stream containing the Range to restore.
   * @todo call update_observers or not ?
   */
  std::istream & Range::fromStream(std::istream & flux)
  {
    _min.fromStream(flux);
    _current.fromStream(flux);
    _consign.fromStream(flux);
    _max.fromStream(flux);

    return flux;
  }


} // namespace hkl
