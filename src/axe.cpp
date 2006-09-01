#include "axe.h"

namespace hkl {

    Axe::Axe(MyString const & name, MyString const & description,
             double min, double current, double max,
             svector const & axe, int direction) throw (HKLException) :
      Parameter(name, description , min , current, max)
    {
      if ( !(axe == svector()) )
          _axe = axe;
      else
          HKLEXCEPTION("Can not create an Axe with a null axe vector.", "Please set a correct axe for this axe.");

      if (direction == 0)
          HKLEXCEPTION("Can not describe an Axe with a null direction", "chose between 1 or -1");
      if (direction > 0) _direction = 1;
      if (direction < 0) _direction = -1;
    }

    bool
    Axe::operator ==(Axe const & axe) const
      {
        return Parameter::operator==(axe)
        && _axe == axe._axe
        && _direction == axe._direction;
      }

    Quaternion
    Axe::asQuaternion(void) const
      {
        double const & angle = get_current().get_value() * _direction / 2.; 
        double s_angle = sin(angle) / _axe.norm2(); 

        return Quaternion(cos(angle), s_angle * _axe[0], s_angle * _axe[1], s_angle * _axe[2]);
      }

    double
    Axe::getDistance(Axe const & axe) const
      {
        double v1 = fmod(get_current().get_value(), 2 * constant::math::pi);
        double v2 = fmod(axe.get_current().get_value(), 2 * constant::math::pi);

        return acos(cos(v1-v2));
      }

    ostream & 
    Axe::printToStream(ostream & flux) const
      {
        flux  << "\"" << get_name() << "\"";
        flux  << " \"" << get_description() << "\"";
        flux << " " << _axe << ", ";
        flux << showpoint << showpos;
        flux << "Sens de rotation: " << _direction << ", "
        << "Minimum: " << get_min().get_value() *  constant::math::radToDeg << ", "
        << "Current: " << get_current().get_value() * constant::math::radToDeg << ", "
        << "Maximum: " << get_max().get_value() * constant::math::radToDeg << endl;
        flux << noshowpoint << noshowpos << dec;

        return flux;
      }

    ostream &
    Axe::toStream(ostream & flux) const
      {
        Parameter::toStream(flux);
        _axe.toStream(flux);
        flux << " " << _direction << endl;

        return flux;    
      }

    istream &
    Axe::fromStream(istream & flux)
      {
        Parameter::fromStream(flux);
        _axe.fromStream(flux);
        flux >> _direction;

        return flux;
      }

} // namespace hkl
