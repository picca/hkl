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
  {
    if (!_geometry.isValid())
      HKLEXCEPTION("Can not create a reflection from an invalid geometry", "Check the geometry.");
  }

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
             && _flag == reflection._flag;
    }

  Value
  Reflection::computeAngle(svector const & hkl) const
    {
      double dot_product = _hkl.scalar(_hkl);

      double length1 = _hkl.norm2();
      double length2 = hkl.norm2();
      double cosine = dot_product / (length1*length2);

      return Value(acos(cosine));
    }

  bool
  Reflection::isColinear(Reflection const & reflection) const
    {
      if ((_hkl.vectorialProduct(reflection._hkl)).norm2() < constant::math::epsilon_1)
        return true;
      else
        return false;
    }


  ostream &
  Reflection::printToStream(ostream & flux) const
    {
      flux << _hkl;
      vector<hkl::MyString> axesNames = _geometry.getAxesNames();

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
      flux << " | " << "(" << _flag << ") ";

      return flux;
    }

  ostream &
  Reflection::toStream(ostream & flux) const
    {
      _geometry.toStream(flux);
      _hkl.toStream(flux);
      flux << setprecision(constant::math::precision);
      flux << " " << _flag;

      return flux;
    }

  istream &
  Reflection::fromStream(istream & flux)
  {
    _geometry.fromStream(flux);
    _hkl.fromStream(flux);
    flux >> setprecision(constant::math::precision);
    flux >> _flag;

    return flux;
  }

} // namespace hkl
