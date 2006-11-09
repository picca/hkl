#include "reflection_monocrystal.h"

using namespace std;

namespace hkl
  {
  namespace reflection
    {

    MonoCrystal::MonoCrystal(Geometry const & geometry,
                             svector const & hkl,
                             bool const & flag) :
        Reflection(geometry, hkl, flag)
    {
      // do not forgot to update _hkl_phi
      _hkl_phi = _geometry.getSampleRotationMatrix().transpose() * _geometry.getQ();
    }

    MonoCrystal::MonoCrystal(MonoCrystal const & reflection) :
        Reflection(reflection)
    {
      // update the _hkl_phi fitness helper
      //_hkl_phi = _geometry.getSampleRotationMatrix().transpose() * _geometry.getQ();
    }

    MonoCrystal::~MonoCrystal(void)
    {}

    Reflection *
    MonoCrystal::clone(void) const
      {
        return new MonoCrystal(*this);
      }

  } // namespace reflection
} // namespace hkl
