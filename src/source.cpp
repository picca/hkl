
#include "source.h"

namespace hkl
  {

  /**
   * @brief Default Constructor.
   *
   * Create a new Source with all is privates parameters set to zero.
   * After this you must set the waveLength before using it, with the
   * setWaveLength method.
   */
  Source::Source() :
      _waveLength(1.54),
      _direction(svector(1,0,0)),
      _qi(0, constant::physic::tau / 1.54, 0, 0)
  {
  }

  /**
   * @brief Constructor from parameters
   * @param waveLength the wavelength of the beam.
   * @param direction the X-rays beam direction. This parameter is normalize.
   *
   * Create a new Source from the parameters.
   * <b>_waveLength unit must be consistent with the crystal length units</b>.
   */
  Source::Source(const hkl::Value & waveLength, const hkl::svector & direction)
  {
    _waveLength = waveLength;
    _direction = direction.normalize();

    double k = constant::physic::tau / _waveLength.get_value();
    svector ki(_direction);
    ki *= k;
    _qi = Quaternion(0., ki.x(), ki.y(), ki.z());
  }

  /**
   * @brief set the wavelength
   * @param waveLength the wavelength to set.
   * @exception HKLException if waveLength == 0.
   *
   * Set the waveLength of the source
   * <b>wl unit must be consistent with the crystal length units</b>.
   */
  void Source::setWaveLength(const hkl::Value & waveLength) throw(hkl::HKLException)
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

  /**
   * @brief Set the _{p0} of the Source.
   * @param direction to set
   *
   * The direction is normalize.
   */
  void Source::setDirection(const hkl::svector & direction) throw(hkl::HKLException)
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

  /**
   * @brief Get the ki vector
   */
  hkl::svector Source::getKi() const
    {
      double k = constant::physic::tau / _waveLength;

      svector ki(_direction);
      ki *= k;

      return ki;
    }

  /**
   * @brief set the ki vector
   */
  void Source::setKi(const hkl::svector & ki) throw(hkl::HKLException)
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

  /**
   * \brief Are two Source equals ?
   * \param source the hkl::Source to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool Source::operator==(const hkl::Source & source) const
    {
      return _waveLength == source._waveLength
             && _direction == source._direction
             && _qi == source._qi;
    }

  /**
   * @brief print the Source into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Source::printToStream(std::ostream & flux) const
    {
      flux << "Source: "
      << "Wave length = " << _waveLength << ", "
      << "Direction = " << _direction << ", "
      << "Qi = " << _qi;

      return flux;
    }

  /**
   * @brief print on a stream the content of the Source
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Source::toStream(std::ostream & flux) const
    {
      _waveLength.toStream(flux);
      _direction.toStream(flux);
      _qi.toStream(flux);

      return flux;
    }

  /**
   * @brief restore the content of the Source from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Source::fromStream(std::istream & flux)
  {
    _waveLength.fromStream(flux);
    _direction.fromStream(flux);
    _qi.fromStream(flux);

    return flux;
  }


} // namespace hkl
