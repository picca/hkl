#include "source.h"

namespace hkl
  {

  Source::Source(void) :
      _waveLength(1.54),
      _direction(svector(1, 0, 0)),
      _qi(Quaternion())
  {}

  Source::Source(Source const & source) :
      _waveLength(source._waveLength),
      _direction(source._direction),
      _qi(source._qi)
  {}

  Source::Source(Value const & waveLength, svector const & direction)
  {
    _waveLength = waveLength;
    _direction = direction.normalize();

    double k = constant::physic::tau / _waveLength.get_value();
    svector ki(_direction);
    ki *= k;
    _qi = Quaternion(0., ki.x(), ki.y(), ki.z());
  }

  bool
  Source::operator ==(Source const & source) const
    {
      return _waveLength == source._waveLength
             && _direction == source._direction
             && _qi == source._qi;
    }

  void
  Source::setWaveLength(Value const & waveLength) throw (HKLException)
  {
    if (fabs(waveLength) < constant::math::epsilon)
      HKLEXCEPTION("Cannot set a source with a null wave length",
                   "Please set a non-null wave length");
    else
      {
        _waveLength = waveLength;

        if (!(_direction == svector()))
          {
            double k = constant::physic::tau / _waveLength;
            svector ki(_direction);
            ki *= k;
            _qi = Quaternion(0., ki.x(), ki.y(), ki.z());
          }
      }
  }

  void
  Source::setDirection(svector const & direction) throw (HKLException)
  {
    if (direction == svector())
      HKLEXCEPTION("Cannot set a source with a null direction.", "Please set a non-null direction.");
    else
      {
        _direction = direction.normalize();
        svector ki(_direction);

        if (_waveLength > constant::math::epsilon)
          {
            double k = constant::physic::tau / _waveLength;
            ki *= k;
          }
        _qi = Quaternion(0., ki.x(), ki.y(), ki.z());
      }
  }

  svector
  Source::getKi(void) const
    {
      double k = constant::physic::tau / _waveLength;

      svector ki(_direction);
      ki *= k;

      return ki;
    }

  void
  Source::setKi(svector const & ki) throw (HKLException)
  {
    if (ki == svector())
      HKLEXCEPTION("Cannot set a source with a null Ki", "Please use a non-null Ki.");
    else
      {
        _waveLength = constant::physic::tau / ki.norm2();
        _direction = ki.normalize();

        _qi = Quaternion(0., ki.x(), ki.y(), ki.z());
      }
  }

  ostream &
  Source::printToStream(ostream& flux) const
    {
      flux << "Source: "
      << "Wave length = " << _waveLength << ", "
      << "Direction = " << _direction << ", "
      << "Qi = " << _qi;

      return flux;
    }

  ostream &
  Source::toStream(ostream & flux) const
    {
      _waveLength.toStream(flux);
      _direction.toStream(flux);
      _qi.toStream(flux);

      return flux;
    }

  istream &
  Source::fromStream(istream & flux)
  {
    _waveLength.fromStream(flux);
    _direction.fromStream(flux);
    _qi.fromStream(flux);

    return flux;
  }

} // namespace hkl

