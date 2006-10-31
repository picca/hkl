#include "range.h"

namespace hkl {

    Range::Range(void) :
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
            HKLEXCEPTION("The current range is not valid", "set a valid range");
      }

    void
    Range::set_current(Value const & current) throw (HKLException)
      {
        if (_min <= current && current <= _max)
            _current = current;
        else
          {
            ostringstream reason;
            reason << "Can not set this current value : " << current.get_value()
            << " outside (" << _min.get_value() << ":" << _max.get_value() << ")";
            HKLEXCEPTION(reason.str(), "Change the current value or the minimun and maximum range.");
          }
      }

    void
    Range::set_range(Value const & min, Value const & max) throw (HKLException)
      {
        if (min <= _current)
            _min = min;
        else
            HKLEXCEPTION("Can not set a minimum greater than the current value.", "Change the current value or the minimun range.");
        if (_current <= max)
            _max = max;
        else
            HKLEXCEPTION("Can not set a maximum lower than the current value.", "Change the current value or the maximum range.");
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
        << "Range : " << _current
        << " Min: " << _min
        << " Max: " << _max << endl;

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
