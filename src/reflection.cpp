#include <math.h>
#include <iomanip>

#include "reflection.h"

using namespace std;

namespace hkl
  {

  Reflection::Reflection(Geometry const & geometry,
                         svector const & hkl,
                         bool const & flag) throw (HKLException) :
      _geometry(geometry),
      _hkl(hkl),
      _flag(flag)
  {}

  Reflection::Reflection(Reflection const & reflection) :
      _geometry(reflection._geometry),
      _hkl(reflection._hkl),
      _flag(reflection._flag),
      _hkl_phi(reflection._hkl_phi)
  {}

  Reflection::~Reflection(void)
  {}

  bool
  Reflection::operator == (Reflection const & reflection) const
    {
      return _geometry == reflection._geometry
             && _hkl == reflection._hkl
             && _flag == reflection._flag
             && _hkl_phi == reflection._hkl_phi;
    }

  Value
  Reflection::computeAngle(svector const & hkl) const
    {
      return Value(_hkl.angle(hkl));
    }

  bool
  Reflection::isColinear(Reflection const & reflection) const
    {
      if ((_hkl.vectorialProduct(reflection._hkl)).norm2() < constant::math::epsilon)
        return true;
      else
        return false;
    }


  ostream &
  Reflection::printToStream(ostream & flux) const
    {
      flux << _hkl;
      vector<string> axesNames = _geometry.getAxesNames();

      unsigned int nb_axes = axesNames.size();
      unsigned int i;
      for(i=0; i<nb_axes; i++)
        {
          flux.width(9);
          flux << _geometry.get_axe(axesNames[i]).get_current().get_value() * hkl::constant::math::radToDeg;
        }
      flux << " |";
      flux.width(9);
      flux << _geometry.get_source().get_waveLength().get_value();
      flux << " | " << "(" << _flag << ") hkl_phi : " << _hkl_phi;

      return flux;
    }

  ostream &
  Reflection::toStream(ostream & flux) const
    {
      _geometry.toStream(flux);
      _hkl.toStream(flux);
      _hkl_phi.toStream(flux);
      flux << " " << _flag;

      return flux;
    }

  istream &
  Reflection::fromStream(istream & flux)
  {
    _geometry.fromStream(flux);
    _hkl.fromStream(flux);
    _hkl_phi.fromStream(flux);
    flux >> _flag;

    return flux;
  }

} // namespace hkl
