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
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _RANGE_H
#define _RANGE_H


#include "value.h"
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {

  class Range
    {
    protected:
      hkl::Value _min;

      hkl::Value _current;

      hkl::Value _consign;

      hkl::Value _max;


    public:
      /**
       * @brief The default constructor.
       */
      Range();

      /**
       * @brief constructor of the Range class.
       *
       * @param min The minimum value of the Range.
       * @param current The current value of the Range.
       * @param consign The consign value of the Range.
       * @param max The maximum value of the Range.
       * @throw HKLException if not min < current, consign < max;
       */
      Range(const hkl::Value & min, const hkl::Value & current, const hkl::Value & consign, const hkl::Value & max) throw(hkl::HKLException);

      Range(const Range & source);

      /**
       * @brief Get the _min Value of the Range class.
       * @return The minimum Value.
       */
      inline hkl::Value const & get_min() const;

      /**
       * @brief Get the _current Value of the Range class.
       * @return The current Value.
       */
      inline const hkl::Value & get_current() const;

      inline hkl::Value const & get_consign() const;

      /**
       * @brief Get the _max Value of the Range class.
       * @return The maximum Value.
       */
      inline hkl::Value const & get_max() const;

      /**
       * @brief Set the _current hkl::Value of the Range class.
       * @param current The hkl::Value to set.
       * @throw An HKLException if the current hkl::Value in not between min and max.
       */
      void set_current(const hkl::Value & current) throw(hkl::HKLException);

      /**
       * @brief Set the _current double of the Range class.
       * @param current The double to set.
       *
       * This method do not check for the validity of the Range. This method
       * is requiered by the simplex affinement.
       */
      void set_current(double current);

      /**
       * @brief Set the consign hkl::Value of the Range class.
       * @param consign The hkl::Value to set.
       * @throw An HKLException if the consign hkl::Value in not in between min and max.
       */
      void set_consign(const hkl::Value & consign) throw(hkl::HKLException);

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
      void set_range(const hkl::Value & min, const hkl::Value & max) throw(hkl::HKLException);

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
      void set(double min, double current, double consign, double max);

      /**
       * @brief Set a Range from another one.
       * @param range The Range to set.
       *
       * this method set only the _min, _current, _consign and _max Value of the Range.
       */
      void set(const Range & range);

      /*!
       * \brief Are two Range equals ?
       * \param range the Range to compare with.
       */

      bool operator==(const Range & range) const;

      /*!
       * \brief print the Range into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /*!
       * \brief Save the Range into a stream.
       * \param flux the stream to save the Range into.
       * \return The stream with the Range.
       */
      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a Range from a stream.
       * \param flux The stream containing the Range to restore.
       * @todo call update_observers or not ?
       */
      std::istream & fromStream(std::istream & flux);

    };
  /**
   * @brief Get the _min Value of the Range class.
   * @return The minimum Value.
   */
  inline hkl::Value const & Range::get_min() const
    {
      return _min;
    }

  /**
   * @brief Get the _current Value of the Range class.
   * @return The current Value.
   */
  inline const hkl::Value & Range::get_current() const
    {
      return _current;
    }

  inline hkl::Value const & Range::get_consign() const
    {
      return _consign;
    }

  /**
   * @brief Get the _max Value of the Range class.
   * @return The maximum Value.
   */
  inline hkl::Value const & Range::get_max() const
    {
      return _max;
    }


} // namespace hkl

/*!
 * \brief Overload of the << operator for the Range clas
 * \param flux The ostream to modify.
 * \param range The range to print.
 *
 * \return the modified ostream
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::Range const & range)
{
  return range.printToStream(flux);
}
#endif
