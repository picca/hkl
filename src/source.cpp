#include <assert.h>

#include "source.h"


inline void source_compute_qi(hkl_quaternion * qi, double waveLength, hkl_svector const * direction)
{
  double k;
  k = HKL_TAU / waveLength;

  qi->data[0] = 0.;
  qi->data[1] = direction->data[0] * k;
  qi->data[2] = direction->data[1] * k;
  qi->data[3] = direction->data[2] * k;
}

namespace hkl
  {

  /**
   * @brief Default Constructor.
   *
   * Create a new Source with all is privates parameters set to zero.
   * After this you must set the waveLength before using it, with the
   * setWaveLength method.
   */
  Source::Source()
  {
    hkl_svector default_direction = {{1, 0, 0}};
    hkl_quaternion default_qi = {{0, HKL_TAU / HKL_SOURCE_DEFAULT_WAVELENGTH, 0, 0}};

    _waveLength.set_value(HKL_SOURCE_DEFAULT_WAVELENGTH);
    _direction = default_direction;
    _qi = default_qi;
  }

  /**
   * @brief Constructor from parameters
   * @param waveLength the wavelength of the beam.
   * @param direction the X-rays beam direction. This parameter is normalize.
   *
   * Create a new Source from the parameters.
   * <b>_waveLength unit must be consistent with the crystal length units</b>.
   */
  Source::Source(const hkl::Value & waveLength, const hkl_svector * direction)
  {
    // check the input parameters validity
    assert(waveLength > 0);
    assert(::hkl_svector_norm2(direction) > HKL_EPSILON);

    _waveLength = waveLength;

    _direction = *direction;
    ::hkl_svector_normalize(&_direction);

    ::source_compute_qi(&_qi, _waveLength.get_value(), &_direction);
    // compute the qi quaternion 
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
        ::source_compute_qi(&_qi, _waveLength.get_value(), &_direction);
  }

  /**
   * @brief Set the _{p0} of the Source.
   * @param direction to set
   *
   * The direction is normalize.
   */
  void Source::setDirection(const hkl_svector * direction) throw(hkl::HKLException)
  {
    if (::hkl_svector_norm2(direction) < HKL_EPSILON)
      HKLEXCEPTION("Cannot set a source with a null direction.", "Please set a non-null direction.");
    else
      {
        _direction = *direction;
        ::hkl_svector_normalize(&_direction);

        ::source_compute_qi(&_qi, _waveLength.get_value(), &_direction);
      }
  }

  /**
   * @brief Get the ki vector
   */
  void Source::get_ki(hkl_svector * ki) const
    {
      double k;

      *ki = _direction;
      k = HKL_TAU / _waveLength;
      ::hkl_svector_times_double(ki, k);
    }

  /**
   * @brief set the ki vector
   */
  void Source::set_ki(const hkl_svector * ki) throw(hkl::HKLException)
  {
    double norm2 = ::hkl_svector_norm2(ki);

    if (norm2 < HKL_EPSILON)
      HKLEXCEPTION("Cannot set a source with a null Ki", "Please use a non-null Ki.");
    else
      {
        _waveLength = HKL_TAU / norm2;

        _direction = *ki;
        ::hkl_svector_normalize(&_direction);

        ::hkl_quaternion_from_svector(&_qi, ki);
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
             && ::hkl_svector_cmp(&_direction, &source._direction)
             && ::hkl_quaternion_cmp(&_qi, &source._qi);
    }

  /**
   * @brief print the Source into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Source::printToStream(std::ostream & flux) const
    {
      flux << "Source: "
      << "Wave length = " << _waveLength << ", ";

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

    return flux;
  }
} // namespace hkl
