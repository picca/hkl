#include "range.h"

namespace hkl {

  Range::Range(void)
  {}

  Range::Range(std::string const & name, double value, double min, double max)
    : Value(name, value)
    {
      set_min(min);
      set_max(max);
    }

  Range::Range(Range const & range)
    : Value(range.get_name(), range.get_value())
    {
      set_min(range.get_min());
      set_max(range.get_max());
    }

  Range::~Range(void)
  {}


  bool
    Range::operator == (Range const & range) const
    {
      return Value::operator==(range)
        && fabs(get_min() - range.get_min()) < constant::math::epsilon_1
        && fabs(get_max() - range.get_max()) < constant::math::epsilon_1;  
    }

  std::ostream & 
    Range::printToStream(std::ostream & flux) const
    { 
      Value::printToStream(flux);

      flux  << " Min: " << get_min()
        << " Max: " << get_max();

      return flux;
    }
} // namespace hkl

std::ostream &
operator<< (std::ostream & flux, hkl::Range const & range)
{
  return range.printToStream(flux);
}
