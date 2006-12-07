#include "hklobject.h"

namespace hkl
  {

  HKLObject::HKLObject(MyString const & name, MyString const & description) throw (HKLException)
      : ObjectReadOnly(name, description)
  {}

  HKLObject::~HKLObject(void)
  {}

  bool
  HKLObject::operator == (HKLObject const & hklObject) const
    {
      return ObjectReadOnly::operator==(hklObject)
             && _parameters == hklObject._parameters;
    }

  ostream &
  HKLObject::printToStream(ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      _parameters.printToStream(flux);

      return flux;
    }

  ostream &
  HKLObject::toStream(ostream & flux) const
    {
      ObjectReadOnly::toStream(flux);
      _parameters.toStream(flux);

      return flux;
    }

  istream &
  HKLObject::fromStream(istream & flux)
  {
    ObjectReadOnly::fromStream(flux);
    _parameters.fromStream(flux);

    return flux;
  }

} // namespace hkl
