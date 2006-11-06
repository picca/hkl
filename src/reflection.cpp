#include <math.h>
#include <iomanip>

#include "reflection.h"

using namespace std;

namespace hkl
  {

  Reflection::Reflection(Geometry const & geometry,
                         Value const & h,
                         Value const & k,
                         Value const & l,
                         bool const & flag) throw (HKLException) :
      _geometry(geometry),
      _h(h),
      _k(k),
      _l(l),
      _flag(flag)
  {
    if (!_geometry.isValid())
      HKLEXCEPTION("Can not create a reflection from an invalid geometry", "Check the geometry.");
  }

  Reflection::Reflection(Reflection const & reflection) :
      _geometry(reflection._geometry),
      _h(reflection._h),
      _k(reflection._k),
      _l(reflection._l),
      _flag(reflection._flag),
      _hkl_phi(reflection._hkl_phi)
{
}

  Reflection::~Reflection(void)
  {}

  bool
  Reflection::operator == (Reflection const & reflection) const
    {
      return _geometry == reflection._geometry
             && _h == reflection._h
             && _k == reflection._k
             && _l == reflection._l
             && _flag == reflection._flag;
    }

  svector const
  Reflection::get_hkl(void) const
    {
      return svector(_h.get_value(), _k.get_value(), _l.get_value());
    }

  Value
  Reflection::computeAngle(Value const & h, Value const & k, Value const & l) const
    {
      double dot_product = h.get_value() * _h.get_value()
                           + k.get_value() * _k.get_value()
                           + l.get_value() * _l.get_value();

      double length1 = sqrt(_h.get_value()*_h.get_value() + _k.get_value()*_k.get_value() + _l.get_value()*_l.get_value());
      double length2 = sqrt(h.get_value()*h.get_value() + k.get_value()*k.get_value() + l.get_value()*l.get_value());
      double cosine = dot_product / (length1*length2);

      return Value(acos(cosine));
    }

  bool
  Reflection::isColinear(Reflection const & reflection) const
    {
      svector v1(_h.get_value(), _k.get_value(), _l.get_value());
      svector v2(reflection._h.get_value(), reflection._k.get_value(), reflection._l.get_value());
      if ((v1.vectorialProduct(v2)).norm2() < constant::math::epsilon_1)
        return true;
      else
        return false;
    }


  ostream &
  Reflection::printToStream(ostream & flux) const
    {
      flux.precision(3);
      flux.width(9);
      //flux << showpos;
      flux.width(9);
      flux << _h.get_value();
      flux.width(9);
      flux << _k.get_value();
      flux.width(9);
      flux << _l.get_value();
      flux << " |";

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
      _h.toStream(flux);
      _k.toStream(flux);
      _l.toStream(flux);
      flux << setprecision(constant::math::precision);
      flux << " " << _flag;

      return flux;
    }

  istream &
  Reflection::fromStream(istream & flux)
  {
    _geometry.fromStream(flux);
    _h.fromStream(flux);
    _k.fromStream(flux);
    _l.fromStream(flux);
    flux >> setprecision(constant::math::precision);
    flux >> _flag;

    return flux;
  }

} // namespace hkl
