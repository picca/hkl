#include "reflection_monocrystal.h"

using namespace std;

namespace hkl
  {
  namespace reflection
    {

    MonoCrystal::MonoCrystal(Geometry const & geometry,
                             Value const & h,
                             Value const & k,
                             Value const & l,
                             bool const & flag) :
        Reflection(geometry, h, k, l, flag)
    {
      // do not forgot to update _hkl_phi
      _hkl_phi = _geometry.getSampleRotationMatrix().transpose() * _geometry.getQ();
    }

    MonoCrystal::MonoCrystal(MonoCrystal const & reflection) :
        Reflection(reflection)
    {
      // update the _hkl_phi fitness helper
      _hkl_phi = _geometry.getSampleRotationMatrix().transpose() * _geometry.getQ();
    }

    MonoCrystal::~MonoCrystal(void)
    {}

    Reflection *
    MonoCrystal::clone(void) const
      {
        return new MonoCrystal(*this);
      }

    ostream &
    MonoCrystal::printToStream(ostream & flux) const
      {
        Reflection::printToStream(flux);
        flux << "_hkl_phi : " << _hkl_phi << endl;

        return flux;
      }

    bool
    MonoCrystal::operator == (MonoCrystal const & reflection) const
      {
        return Reflection::operator==(reflection)
               && _hkl_phi == reflection._hkl_phi;
      }

    ostream &
    MonoCrystal::toStream(ostream & flux) const
      {
        _geometry.toStream(flux);
        _h.toStream(flux);
        _k.toStream(flux);
        _l.toStream(flux);
        flux << setprecision(constant::math::precision);
        flux << " " << _flag;
        //m_hkl_phi.toStream(flux);

        return flux;
      }

    istream &
    MonoCrystal::fromStream(istream & flux)
    {
      _geometry.fromStream(flux);
      _h.fromStream(flux);
      _k.fromStream(flux);
      _l.fromStream(flux);
      flux >> setprecision(constant::math::precision);
      flux >> _flag;
      //m_hkl_phi.fromStream(flux);

      return flux;
    }

  } // namespace reflection
} // namespace hkl
