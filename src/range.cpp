#include "range.h"

namespace hkl
  {

  Range::Range(void) :
      Observable(),
      _min(Value()),
      _current(Value()),
      _max(Value())
  {}

  Range::Range(Value const & min, Value const & current, Value const & max) throw (HKLException)
  {
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
  }

  void
  Range::set_current(Value const & current) throw (HKLException)
  {
    if (_min <= current && current <= _max)
      {
        _current = current;
        set_changed();
        update_observers();
      }
    else
      {
        ostringstream reason;
        reason << "Can not set this current value : " << current.get_value()
        << " outside (" << _min.get_value() << ":" << _max.get_value() << ")";
        HKLEXCEPTION(reason.str(), "Change the current value or the minimun and maximum range.");
      }
  }

  /**
  * @brief 
  * 
  * @param value 
  * @param throw(HKLException 
  *
  * just to faster the simplex method
  */
  void
  Range::set_current(double const & value)
  {
    _current.set_value(value);
    set_changed();
    update_observers();
  }

  void
  Range::set_range(Value const & min, Value const & max) throw (HKLException)
  {
    if (min <= _current)
      _min = min;
    else
      {
        ostringstream reason;
        reason << "Can not set a minimum (" << min << ") greater than the current value (" << _current << ")";
        HKLEXCEPTION(reason.str(), "Change the current value or the minimun range.");
      }

    if (_current <= max)
      _max = max;
    else
      {
        ostringstream reason;
        reason << "Can not set a maximum (" << max << ") lower than the current value (" << _current << ")";
        HKLEXCEPTION(reason.str(), "Change the current value or the minimun range.");
      }
    set_changed();
    update_observers();
  }

  void
  Range::set(double min, double current, double max)
    {
      _min.set_value(min);
      _current.set_value(current);
      _max.set_value(max);
      set_changed();
      update_observers();
    }

  void
  Range::set(Range const & range)
    {
      _min = range._min;
      _current = range._current;
      _max = range._max;
      set_changed();
      update_observers();
    }

  Range &
  Range::operator*=(Range const & range)
  {
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

    _min.set_value(min);
    _current *= range._current;
    _max.set_value(max);

    return *this;
  }

  Range &
  Range::operator *=(double const & d)
  {
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
    double current = _current.get_value() * d;
    set(min, current, max);

    return *this;
  }

  bool
  Range::contain_zero(void) const
    {
      if (_min.get_value() <= 0 && _max.get_value() >= 0)
        return true;
      else
        return false;
    }

  bool
  Range::operator == (Range const & range) const
    {
      return _current == range._current
             && _min == range._min
             && _max == range._max;
    }

  ostream &
  Range::printToStream(ostream & flux) const
    {
      flux
      << _current.get_value()
      << " [ " << _min.get_value()
      << " : "
      << _max.get_value() << " ]" << endl;

      return flux;
    }

  ostream &
  Range::toStream(ostream & flux) const
    {
      _current.toStream(flux);
      _min.toStream(flux);
      _max.toStream(flux);

      return flux;
    }

  istream &
  Range::fromStream(istream & flux)
  {
    _current.fromStream(flux);
    _min.fromStream(flux);
    _max.fromStream(flux);

    return flux;
  }

} // namespace hkl

hkl::Range cos(hkl::Range const & range)
{
  hkl::Range res;

  double min = range.get_min().get_value();
  double current = range.get_current().get_value();
  double max = range.get_max().get_value();

  if (max - min >= 2 * hkl::constant::math::pi)
    res.set(-1, cos(current), 1);
  else
    {
      int quad_min = (int)floor(2 * min / hkl::constant::math::pi) % 4;
      if (quad_min < 0)
        quad_min += 4;

      int quad_max = (int)floor(2 * max / hkl::constant::math::pi) % 4;
      if (quad_max < 0)
        quad_max += 4;

      //cout << "quadrant : " << quad_min << ", " << quad_max << endl;
      switch (quad_max)
        {
        case 0:
          switch (quad_min)
            {
            case 0:
              res.set(cos(max), cos(current), cos(min));
              break;
            case 1:
              res.set(-1, cos(current), 1);
              break;
            case 2:
              res.set(cos(min), cos(current), 1);
              break;
            case 3:
              if (cos(min) < cos(max))
                res.set(cos(min), cos(current), 1);
              else
                res.set(cos(max), cos(current), 1);
              break;
            }
          break;
        case 1:
          switch (quad_min)
            {
            case 0:
            case 1:
              res.set(cos(max), cos(current), cos(min));
              break;
            case 2:
              if (cos(min) < cos(max))
                res.set(cos(min), cos(current), 1);
              else
                res.set(cos(max), cos(current), 1);
              break;
            case 3:
              res.set(cos(max), cos(current), 1);
              break;
            }
          break;
        case 2:
          switch (quad_min)
            {
            case 0:
              res.set(-1, cos(current), cos(min));
              break;
            case 1:
              if (cos(min) < cos(max))
                res.set(-1, cos(current), cos(max));
              else
                res.set(-1, cos(current), cos(min));
              break;
            case 2:
              res.set(cos(min), cos(current), cos(max));
              break;
            case 3:
              res.set(-1, cos(current), 1);
              break;
            }
          break;
        case 3:
          switch (quad_min)
            {
            case 0:
              if (cos(min) < cos(max))
                res.set(-1, cos(current), cos(max));
              else
                res.set(-1, cos(current), cos(min));
              break;
            case 1:
              res.set(-1, cos(current), cos(max));
              break;
            case 2:
            case 3:
              res.set(cos(min), cos(current), cos(max));
              break;
            }
          break;
        }
    }
  //cout << "cos   : " << res << endl;
  return res;
}

hkl::Range acos(hkl::Range const & range)
{
  double min = acos(range.get_max().get_value());
  double current = acos(range.get_current().get_value());
  double max = acos(range.get_min().get_value());

  return hkl::Range(min, current, max);
}
