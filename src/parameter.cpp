#include "parameter.h"

using namespace std;

namespace hkl {

    Parameter::Parameter(MyString const & name, MyString const & description,
                         Value const & current, Value const & min, Value const &  max) throw (HKLException):
      ObjectReadOnly(name, description),
      Range(current, min, max)
    {}

    bool
    Parameter::operator==(Parameter const & parameter) const
      {
        return ObjectReadOnly::operator==(parameter)
        && Range::operator==(parameter);
      }

    ostream &
    Parameter::printToStream(ostream & flux) const
      {
        ObjectReadOnly::printToStream(flux);
        Range::printToStream(flux);
        return flux;
      }

    ostream &
    Parameter::toStream(ostream & flux) const
      {
        ObjectReadOnly::toStream(flux);
        Range::toStream(flux);
        return flux;
      }

    istream &
    Parameter::fromStream(istream & flux)
      {
        ObjectReadOnly::fromStream(flux);
        Range::fromStream(flux);
        return flux;
      }

} // namespace hkl
