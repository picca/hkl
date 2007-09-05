
#include <cmath>
#include "interval.h"

namespace hkl {

/**
 * @brief build an empty Interval
 */
Interval::Interval() :
  _min(0),
  _max(0)
{
  // Bouml preserved body begin 00042402
  // Bouml preserved body end 00042402
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
  // Bouml preserved body begin 00042982
  if (_min > _max)
    HKLEXCEPTION("can not create such an interval", "min > max");
  // Bouml preserved body end 00042982
}

Interval::~Interval() 
{
  // Bouml preserved body begin 00042482
  // Bouml preserved body end 00042482
}

Interval::Interval(const hkl::Interval & source) :
  _min(source._min),
  _max(source._max)
{
  // Bouml preserved body begin 00042502
  // Bouml preserved body end 00042502
}

/**
 * @brief change the interval with the min and max value set.
 * @param min the minimum value to set.
 * @param max the maximum value to set.
 * @throw HKLException if min > max.
 */
void Interval::set(double min, double max) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00042A02
  if (min <= max)
  {
    _min = min;
    _max = max;
  }
  else
    HKLEXCEPTION("can not create such an interval", "min > max");
  // Bouml preserved body end 00042A02
}

/**
 * @brief Add a Interval to another one.
 * @param interval The Interval to add.
 * @return A Interval ref on the Interval after the addition.
 */
hkl::Interval & Interval::operator+=(const hkl::Interval & interval) 
{
  // Bouml preserved body begin 00041E02
    _min += interval._min;
    _max += interval._max;

    return *this;
  // Bouml preserved body end 00041E02
}

/**
 * @brief Add a Interval to another one.
 * @param value The Interval to add.
 * @return A Interval ref on the Interval after the addition.
 */
hkl::Interval & Interval::operator+=(const double & value) 
{
  // Bouml preserved body begin 00041E82
      _min += value;
      _max += value;
  
      return *this;
  // Bouml preserved body end 00041E82
}

/**
 * @brief Substract a Interval to another one.
 * @param interval The Interval to substract.
 * @return A Interval ref on the Interval after the substraction.
 * @todo test
 */
hkl::Interval & Interval::operator-=(const hkl::Interval & interval) 
{
  // Bouml preserved body begin 00042302
      _min -= interval._max;
      _max -= interval._min;
  
      return *this;
  // Bouml preserved body end 00042302
}

/**
 * @brief Substract a Interval to another one.
 * @param value The Interval to substract.
 * @return A Interval ref on the Interval after the substraction.
 * @todo test
 */
hkl::Interval & Interval::operator-=(const double & value) throw(hkl::Affinement) 
{
  // Bouml preserved body begin 00042382
      _min -= value;
      _max -= value;
  
      return *this;
  // Bouml preserved body end 00042382
}

/**
 * @brief Multiply a Interval by another one.
 * @param interval The Interval to multiply by.
 * @return A Interval ref on the Interval after the multiplication.
 */
hkl::Interval & Interval::operator*=(const hkl::Interval & interval) 
{
  // Bouml preserved body begin 00024F02
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
  // Bouml preserved body end 00024F02
}

/**
 * @brief Multiply a Interval by a double value.
 * @param d The double value.
 * @return The Interval after the multiplication.
 */
hkl::Interval & Interval::operator*=(double d) 
{
  // Bouml preserved body begin 00024F82
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
  // Bouml preserved body end 00024F82
}

/**
 * @brief Divide a Interval by a double value.
 * @param d The double value.
 * @return The Interval divided.
 */
hkl::Interval & Interval::operator/=(const double & d) 
{
  // Bouml preserved body begin 00041982
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
  // Bouml preserved body end 00041982
}

/**
 * @brief check if the Interval contain zero.
 * @return true if zero is include in between min, max.
 */

bool Interval::contain_zero() const 
{
  // Bouml preserved body begin 00025002
        if (_min <= 0 && _max >= 0)
          return true;
        else
          return false;
  // Bouml preserved body end 00025002
}

/**
 * @brief compute the cos of the Interval.
 * @return The cosinus of the Interval
 */
hkl::Interval & Interval::cos() 
{
  // Bouml preserved body begin 00041B02
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
  // Bouml preserved body end 00041B02
}

/**
 * @brief compute the acos of the Interval.
 * @return The invert cosinus of the Interval
 */
hkl::Interval & Interval::acos() 
{
  // Bouml preserved body begin 00041B82
    double min = ::acos(_max);
    double max = ::acos(_min);
    _min = min;
    _max = max;

    return *this;
  // Bouml preserved body end 00041B82
}

/**
 * @brief compute the sinus of the Interval.
 * @return the sinus of the Interval
 */
hkl::Interval & Interval::sin() 
{
  // Bouml preserved body begin 00041C02
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

        switch (quad_max) {
          case 0:
            switch (quad_min) {
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
            switch (quad_min) {
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
            switch (quad_min) {
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
            switch (quad_min) {
              case 0:
                _min = -1;
                _max = 1;
                break;
              case 1:
                _min = -1;
                _max = smin;
                break;
              case 2:
                if(smin < smax)
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
                if(smin < smax)
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
  // Bouml preserved body end 00041C02
}

/**
 * @brief compute the invert sinus of the Interval.
 * @return the invert sinus of the Interval
 */
hkl::Interval & Interval::asin() 
{
  // Bouml preserved body begin 00041C82
    double min = ::asin(_min);
    double max = ::asin(_max);
    _min = min;
    _max = max;

    return *this;
  // Bouml preserved body end 00041C82
}

/**
 * @brief compute the tangente of the Interval.
 * @return The tangente of the Interval
 */
hkl::Interval & Interval::tan() 
{
  // Bouml preserved body begin 00041D02
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
  // Bouml preserved body end 00041D02
}

/**
 * @brief compute the invert tangente of the Interval.
 * @return The invert tangente of the Interval
 */
hkl::Interval & Interval::atan() 
{
  // Bouml preserved body begin 00041D82
    double min = ::atan(_min);
    double max = ::atan(_max);
    _min = min;
    _max = max;

    return *this;
  // Bouml preserved body end 00041D82
}

/**
 * @brief Are two Interval equals ?
 * @param interval the hkl::Interval to compare with.
 */
bool Interval::operator==(const hkl::Interval & interval) const 
{
  // Bouml preserved body begin 00042782
      return _min == interval._min
             && _max == interval._max;
  // Bouml preserved body end 00042782
}

/*!
 * \brief print the Interval into a flux
 * \param flux The stream to print into.
 */
std::ostream & Interval::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00042902
        flux
        << "[" << _min
        << " : "
        << _max << "]";
        
        return flux;
  // Bouml preserved body end 00042902
}

/*!
 * \brief Save the Interval into a stream.
 * \param flux the stream to save the Interval into.
 * \return The stream with the Interval.
 */
std::ostream & Interval::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00042882
  flux << " " << _min << " " << _max;
        
  return flux;
  // Bouml preserved body end 00042882
}

/*!
 * \brief Restore a Interval from a stream.
 * \param flux The stream containing the Interval to restore.
 * @todo call update_observers or not ?
 */
std::istream & Interval::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00042802
  flux >> _min >> _max;
        
  return flux;
  // Bouml preserved body end 00042802
}


} // namespace hkl
