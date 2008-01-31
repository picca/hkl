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
#ifndef _HKL_INTERVAL_H
#define _HKL_INTERVAL_H

#include <ostream>
#include <istream>
#include "HKLException.h"
#include "affinement.h"
#include "portability.h"

namespace hkl
  {

  /**
   * @brief A class use to do numerical interval computation.
   */
  class Interval
    {
    protected:
      /**
       * @brief the minimum of the interval
       */
      double _min;

      /**
       * @brief the maximum of the interval
       */
      double _max;


    public:
      /**
       * @brief build an empty Interval
       */
      Interval();

      /**
       * @brief construct a new interval with the min and max value set.
       * @param min the minimum value to set.
       * @param max the maximum value to set.
       * @throw HKLException if min > max.
       */
      Interval(double min, double max) throw(hkl::HKLException);

      virtual ~Interval();

      Interval(const Interval & source);

      inline const double get_min() const;

      inline void set_min(const double & value) throw(hkl::HKLException);

      inline const double get_max() const;

      inline void set_max(const double & value) throw(hkl::HKLException);

      /**
       * @brief change the interval with the min and max value set.
       * @param min the minimum value to set.
       * @param max the maximum value to set.
       * @throw HKLException if min > max.
       */
      void set(double min, double max) throw(hkl::HKLException);

      /**
       * @brief Add a Interval to another one.
       * @param interval The Interval to add.
       * @return A Interval ref on the Interval after the addition.
       */
      Interval & operator+=(const Interval & interval);

      /**
       * @brief Add a Interval to another one.
       * @param value The Interval to add.
       * @return A Interval ref on the Interval after the addition.
       */
      Interval & operator+=(const double & value);

      /**
       * @brief Substract a Interval to another one.
       * @param interval The Interval to substract.
       * @return A Interval ref on the Interval after the substraction.
       * @todo test
       */
      Interval & operator-=(const Interval & interval);

      /**
       * @brief Substract a Interval to another one.
       * @param value The Interval to substract.
       * @return A Interval ref on the Interval after the substraction.
       * @todo test
       */
      Interval & operator-=(const double & value) throw(hkl::Affinement);

      /**
       * @brief Multiply a Interval by another one.
       * @param interval The Interval to multiply by.
       * @return A Interval ref on the Interval after the multiplication.
       */
      Interval & operator*=(const Interval & interval);

      /**
       * @brief Multiply a Interval by a double value.
       * @param d The double value.
       * @return The Interval after the multiplication.
       */
      Interval & operator*=(double d);

      /**
       * @brief Divide a Interval by a double value.
       * @param d The double value.
       * @return The Interval divided.
       */
      Interval & operator/=(const double & d);

      /**
       * @brief check if the Interval contain zero.
       * @return true if zero is include in between min, max.
       */

      bool contain_zero() const;

      /**
       * @brief compute the cos of the Interval.
       * @return The cosinus of the Interval
       */
      Interval & cos();

      /**
       * @brief compute the acos of the Interval.
       * @return The invert cosinus of the Interval
       */
      Interval & acos();

      /**
       * @brief compute the sinus of the Interval.
       * @return the sinus of the Interval
       */
      Interval & sin();

      /**
       * @brief compute the invert sinus of the Interval.
       * @return the invert sinus of the Interval
       */
      Interval & asin();

      /**
       * @brief compute the tangente of the Interval.
       * @return The tangente of the Interval
       */
      Interval & tan();

      /**
       * @brief compute the invert tangente of the Interval.
       * @return The invert tangente of the Interval
       */
      Interval & atan();

      /**
       * @brief Are two Interval equals ?
       * @param interval the Interval to compare with.
       */
      bool operator==(const Interval & interval) const;

      /*!
       * \brief print the Interval into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /*!
       * \brief Save the Interval into a stream.
       * \param flux the stream to save the Interval into.
       * \return The stream with the Interval.
       */
      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a Interval from a stream.
       * \param flux The stream containing the Interval to restore.
       * @todo call update_observers or not ?
       */
      std::istream & fromStream(std::istream & flux);

    };
  inline const double Interval::get_min() const
    {
      return _min;
    }

  inline void Interval::set_min(const double & value) throw(hkl::HKLException)
  {
    if (value <= _max)
      _min = value;
    else
      HKLEXCEPTION("", "");
  }

  inline const double Interval::get_max() const
    {
      return _max;
    }

  inline void Interval::set_max(const double & value) throw(hkl::HKLException)
  {
    if (value >= _min)
      _max = value;
    else
      HKLEXCEPTION("", "");
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
operator<<(std::ostream & flux, hkl::Interval const & interval)
{
  return interval.printToStream(flux);
}
#endif
