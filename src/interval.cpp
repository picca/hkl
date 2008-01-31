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
#include <cmath>
#include "interval.h"

namespace hkl
  {

  /**
   * @brief build an empty Interval
   */
  Interval::Interval() :
      _min(0),
      _max(0)
  {
  }

  /**
   * @brief construct a new interval with the min and max value set.
   * @param min the minimum value to set.
   * @param max the maximum value to set.
   * @throw HKLException if min > max.
   */
  Interval::Interval(double min, double max) throw(hkl::HKLException) :
      _min(min),
      _max(max)
  {
    if (_min > _max)
      HKLEXCEPTION("can not create such an interval", "min > max");
  }

  Interval::~Interval()
  {
  }

  Interval::Interval(const hkl::Interval & source) :
      _min(source._min),
      _max(source._max)
  {
  }

  /**
   * @brief change the interval with the min and max value set.
   * @param min the minimum value to set.
   * @param max the maximum value to set.
   * @throw HKLException if min > max.
   */
  void Interval::set(double min, double max) throw(hkl::HKLException)
  {
    if (min <= max)
      {
        _min = min;
        _max = max;
      }
    else
      HKLEXCEPTION("can not create such an interval", "min > max");
  }

  /**
   * @brief Add a Interval to another one.
   * @param interval The Interval to add.
   * @return A Interval ref on the Interval after the addition.
   */
  hkl::Interval & Interval::operator+=(const hkl::Interval & interval)
  {
    _min += interval._min;
    _max += interval._max;

    return *this;
  }

  /**
   * @brief Add a Interval to another one.
   * @param value The Interval to add.
   * @return A Interval ref on the Interval after the addition.
   */
  hkl::Interval & Interval::operator+=(const double & value)
  {
    _min += value;
    _max += value;

    return *this;
  }

  /**
   * @brief Substract a Interval to another one.
   * @param interval The Interval to substract.
   * @return A Interval ref on the Interval after the substraction.
   * @todo test
   */
  hkl::Interval & Interval::operator-=(const hkl::Interval & interval)
  {
    _min -= interval._max;
    _max -= interval._min;

    return *this;
  }

  /**
   * @brief Substract a Interval to another one.
   * @param value The Interval to substract.
   * @return A Interval ref on the Interval after the substraction.
   * @todo test
   */
  hkl::Interval & Interval::operator-=(const double & value) throw(hkl::Affinement)
  {
    _min -= value;
    _max -= value;

    return *this;
  }

  /**
   * @brief Multiply a Interval by another one.
   * @param interval The Interval to multiply by.
   * @return A Interval ref on the Interval after the multiplication.
   */
  hkl::Interval & Interval::operator*=(const hkl::Interval & interval)
  {
    double m1 = _min * interval._min;
    double m2 = _min * interval._max;
    double m3 = _max * interval._min;
    double m4 = _max * interval._max;

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
    _max = max;

    return *this;
  }

  /**
   * @brief Multiply a Interval by a double value.
   * @param d The double value.
   * @return The Interval after the multiplication.
   */
  hkl::Interval & Interval::operator*=(double d)
  {
    double min;
    double max;
    if (d < 0)
      {
        min = _max * d;
        max = _min * d;
      }
    else
      {
        min = _min * d;
        max = _max * d;
      }
    _min = min;
    _max = max;

    return *this;
  }

  /**
   * @brief Divide a Interval by a double value.
   * @param d The double value.
   * @return The Interval divided.
   */
  hkl::Interval & Interval::operator/=(const double & d)
  {
    double min = _min / d;
    double max = _max / d;
    if (min > max)
      {
        double tmp = min;
        min = max;
        max = tmp;
      }
    _min = min;
    _max = max;

    return *this;
  }

  /**
   * @brief check if the Interval contain zero.
   * @return true if zero is include in between min, max.
   */

  bool Interval::contain_zero() const
    {
      if (_min <= 0 && _max >= 0)
        return true;
      else
        return false;
    }

  /**
   * @brief compute the cos of the Interval.
   * @return The cosinus of the Interval
   */
  hkl::Interval & Interval::cos()
  {
    double cmin = ::cos(_min);
    double cmax = ::cos(_max);

    if (_max - _min >= 2 * M_PI)
      {
        _min = -1;
        _max = 1;
      }
    else
      {
        int quad_min = (int)floor(_min / M_PI_2) % 4;
        if (quad_min < 0)
          quad_min += 4;

        int quad_max = (int)floor(_max / M_PI_2) % 4;
        if (quad_max < 0)
          quad_max += 4;

        switch (quad_max)
          {
          case 0:
            switch (quad_min)
              {
              case 0:
                _min = cmax;
                _max = cmin;
                break;
              case 1:
                _min = -1;
                _max = 1;
                break;
              case 2:
                _min = cmin;
                _max = 1;
                break;
              case 3:
                if (cmin < cmax)
                  {
                    _min = cmin;
                    _max = 1;
                  }
                else
                  {
                    _min = cmax;
                    _max = 1;
                  }
                break;
              }
            break;
          case 1:
            switch (quad_min)
              {
              case 0:
                _min = cmax;
                _max = cmin;
                break;
              case 1:
                _min = -1;
                _max = 1;
                break;
              case 2:
                if (cmin < cmax)
                  {
                    _min = cmin;
                    _max = 1;
                  }
                else
                  {
                    _min = cmax;
                    _max = 1;
                  }
                break;
              case 3:
                _min = cmax;
                _max = 1;
                break;
              }
            break;
          case 2:
            switch (quad_min)
              {
              case 0:
                _min = -1;
                _max = cmin;
                break;
              case 1:
                if (cmin < cmax)
                  {
                    _min = -1;
                    _max = cmax;
                  }
                else
                  {
                    _min = -1;
                    _max = cmin;
                  }
                break;
              case 2:
                if (cmin < cmax)
                  {
                    _min = cmin;
                    _max = cmax;
                  }
                else
                  {
                    _min = -1;
                    _max = 1;
                  }
                break;
              case 3:
                _min = -1;
                _max = 1;
                break;
              }
            break;
          case 3:
            switch (quad_min)
              {
              case 0:
                if (cmin < cmax)
                  {
                    _min = -1;
                    _max = cmax;
                  }
                else
                  {
                    _min = -1;
                    _max = cmin;
                  }
                break;
              case 1:
                _min = -1;
                _max = cmax;
                break;
              case 2:
                _min = cmin;
                _max = cmax;
                break;
              case 3:
                if (cmin < cmax)
                  {
                    _min = cmin;
                    _max = cmax;
                  }
                else
                  {
                    _min = -1;
                    _max = 1;
                  }
                break;
              }
            break;
          }
      }
    return *this;
  }

  /**
   * @brief compute the acos of the Interval.
   * @return The invert cosinus of the Interval
   */
  hkl::Interval & Interval::acos()
  {
    double min = ::acos(_max);
    double max = ::acos(_min);
    _min = min;
    _max = max;

    return *this;
  }

  /**
   * @brief compute the sinus of the Interval.
   * @return the sinus of the Interval
   */
  hkl::Interval & Interval::sin()
  {
    double smin = ::sin(_min);
    double smax = ::sin(_max);

    /* if there is at least one period in b, then a = [-1, 1] */
    if ( _max - _min >= 2 * M_PI)
      {
        _min = -1;
        _max = 1;
      }
    else
      {
        int quad_min = (int)floor(_min / M_PI_2) % 4;
        if (quad_min < 0)
          quad_min += 4;

        int quad_max = (int)floor(_max / M_PI_2) % 4;
        if (quad_max < 0)
          quad_max += 4;

        switch (quad_max)
          {
          case 0:
            switch (quad_min)
              {
              case 0:
                if (smin < smax)
                  {
                    _min = smin;
                    _max = smax;
                  }
                else
                  {
                    _min = -1;
                    _max = 1;
                  }
                break;
              case 3:
                _min = smin;
                _max = smax;
                break;
              case 1:
                if (smin > smax)
                  {
                    _min = -1;
                    _max = smin;
                  }
                else
                  {
                    _min = -1;
                    _max = smax;
                  }
                break;
              case 2:
                _min = -1;
                _max = smax;
                break;
              }
            break;
          case 1:
            switch (quad_min)
              {
              case 0:
                if (smin < smax)
                  {
                    _min = smin;
                    _max = 1;
                  }
                else
                  {
                    _min = smax;
                    _max = 1;
                  }
                break;
              case 1:
                if (smin < smax)
                  {
                    _min = -1;
                    _max = 1;
                  }
                else
                  {
                    _min = smax;
                    _max = smin;
                  }
                break;
              case 2:
                _min = -1;
                _max = 1;
                break;
              case 3:
                _min = smin;
                _max = 1;
                break;
              }
            break;
          case 2:
            switch (quad_min)
              {
              case 0:
                _min = smax;
                _max = 1;
                break;
              case 1:
              case 2:
                if (smin < smax)
                  {
                    _min = -1;
                    _max = 1;
                  }
                else
                  {
                    _min = smax;
                    _max = smin;
                  }
                break;
              case 3:
                if (smin < smax)
                  {
                    _min = smin;
                    _max = 1;
                  }
                else
                  {
                    _min = smax;
                    _max = 1;
                  }
                break;
              }
            break;
          case 3:
            switch (quad_min)
              {
              case 0:
                _min = -1;
                _max = 1;
                break;
              case 1:
                _min = -1;
                _max = smin;
                break;
              case 2:
                if (smin < smax)
                  {
                    _min = -1;
                    _max = smax;
                  }
                else
                  {
                    _min = -1;
                    _max = smin;
                  }
                break;
              case 3:
                if (smin < smax)
                  {
                    _min = smin;
                    _max = smax;
                  }
                else
                  {
                    _min = -1;
                    _max = 1;
                  }
                break;
              }
            break;
          }
      }
    return *this;
  }

  /**
   * @brief compute the invert sinus of the Interval.
   * @return the invert sinus of the Interval
   */
  hkl::Interval & Interval::asin()
  {
    double min = ::asin(_min);
    double max = ::asin(_max);
    _min = min;
    _max = max;

    return *this;
  }

  /**
   * @brief compute the tangente of the Interval.
   * @return The tangente of the Interval
   */
  hkl::Interval & Interval::tan()
  {
    double  tmin = ::tan(_min);
    double  tmax = ::tan(_max);

    int quadrant_down = (int)floor(_min / M_PI_2);
    int quadrant_up = (int)floor(_max / M_PI_2);

    /* if there is at least one period in b or if b contains a Pi/2 + k*Pi, */
    /* then a = ]-oo, +oo[ */
    //std::cout << "min : " << min << "(" << quadrant_down << ") max : " << max << "(" << quadrant_up << ")" << std::endl;
    if ( ((quadrant_up - quadrant_down) >= 2)
         || (!(quadrant_down % 2) && (quadrant_up % 2)) )
      {
        _min = -INFINITY;
        _max = INFINITY;
      }
    else
      {
        _min = tmin;
        _max = tmax;
      }
    return *this;
  }

  /**
   * @brief compute the invert tangente of the Interval.
   * @return The invert tangente of the Interval
   */
  hkl::Interval & Interval::atan()
  {
    double min = ::atan(_min);
    double max = ::atan(_max);
    _min = min;
    _max = max;

    return *this;
  }

  /**
   * @brief Are two Interval equals ?
   * @param interval the hkl::Interval to compare with.
   */
  bool Interval::operator==(const hkl::Interval & interval) const
    {
      return _min == interval._min
             && _max == interval._max;
    }

  /*!
   * \brief print the Interval into a flux
   * \param flux The stream to print into.
   */
  std::ostream & Interval::printToStream(std::ostream & flux) const
    {
      flux
      << "[" << _min
      << " : "
      << _max << "]";

      return flux;
    }

  /*!
   * \brief Save the Interval into a stream.
   * \param flux the stream to save the Interval into.
   * \return The stream with the Interval.
   */
  std::ostream & Interval::toStream(std::ostream & flux) const
    {
      flux << " " << _min << " " << _max;

      return flux;
    }

  /*!
   * \brief Restore a Interval from a stream.
   * \param flux The stream containing the Interval to restore.
   * @todo call update_observers or not ?
   */
  std::istream & Interval::fromStream(std::istream & flux)
  {
    flux >> _min >> _max;

    return flux;
  }


} // namespace hkl
