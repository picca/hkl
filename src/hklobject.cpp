#include "hklobject.h"

namespace hkl {

    HKLObject::HKLObject(MyString const & name, MyString const & description) throw (HKLException)
    : ObjectReadOnly(name, description)
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
        flux << _parameters << endl;

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
