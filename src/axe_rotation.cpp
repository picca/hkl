
#include "axe_rotation.h"
#include "value.h"

namespace hkl
  {

  namespace axe
    {

    /**
     * @brief constructor
     * @param name The name of the Rotation.
     * @param description The description of the Rotation.
     * @param min The minimum of the Rotation.
     * @param current The current position of the Rotation.
     * @param max The maximum value of the Rotation.
     * @param axe The hkl::svector rotation axe coordinates.
     */
    Rotation::Rotation(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, const hkl::svector & axe) throw(hkl::HKLException) :
        hkl::Axe(name, description , min, current, max)
    {
      if (!(axe == svector ()))
        {
          _axe = axe.normalize ();

          double angle;
          double s_angle;
          // update the read_quaternion
          angle = _current.get_value () / 2.;
          s_angle = ::sin(angle);
          _quaternion.set(::cos(angle), s_angle * _axe.x(), s_angle * _axe.y(), s_angle * _axe.z());
          _quaternion_consign.set(::cos(angle), s_angle * _axe.x(), s_angle * _axe.y(), s_angle * _axe.z());
        }
      else
        HKLEXCEPTION ("Can not create an Axe with a null axe vector.",
                      "Please set a correct axe for this axe.");
    }

    Rotation::~Rotation()
    {
    }

    Axe * Rotation::clone() const
      {
        return new hkl::axe::Rotation(*this);
      }

    /**
     * @brief Set the read part of the Rotation
     */
    void Rotation::set_current(const hkl::Value & value)
    {
      // update the _quaternion
      double angle = value.get_value () / 2.;
      double s_angle = ::sin(angle);
      _quaternion.set(::cos(angle), s_angle * _axe.x(), s_angle * _axe.y(), s_angle * _axe.z());
      // call the Axe::set_current to updates all related PseudoAxes
      Axe::set_current(value);
    }

    /**
     * @brief Set the read part of the Rotation
     */
    void Rotation::set_consign(const hkl::Value & value)
    {
      // update the _quaternion_consign
      double angle = value.get_value () / 2.;
      double s_angle = ::sin(angle);
      _quaternion_consign.set(::cos(angle), s_angle * _axe.x(), s_angle * _axe.y(), s_angle * _axe.z());
      // call the Axe::set_consign to updates all related PseudoAxes
      Axe::set_consign(value);
    }

    /**
     * \brief Are two Rotation equals ?
     * \param rotation the Rotation to compare with.
     * \return The comparison of the two Rotation.
     */
    bool Rotation::operator==(const hkl::axe::Rotation & rotation) const
      {
        return Axe::operator== (rotation)
               && _axe == rotation._axe
               && _quaternion == rotation._quaternion
               && _quaternion_consign == rotation._quaternion_consign;
      }

    /**
     * @brief Compute the read distance between two Rotation.
     * @param rotation The hkl::Axe to compute the distance from.
     * @return The distance between the two Rotation.
     */
    double Rotation::get_distance(const hkl::Axe & rotation) const throw(hkl::HKLException)
    {
      if (rotation.get_type() == AXE_ROTATION)
        {
          double v1 = fmod (_current.get_value(), 2 * constant::math::pi);
          double v2 = fmod (rotation.get_current().get_value(), 2 * constant::math::pi);

          return ::acos(::cos(v1 - v2));
        }
      else
        HKLEXCEPTION("Cannot compute the distance between 2 different type of Axes", "Check the constitancy of the geometry.");
    }

    /**
     * @brief Compute the read distance between two Rotation.
     * @param rotation The hkl::Axe to compute the distance from.
     * @return The distance between the two Rotation.
     */
    double Rotation::get_distance_consign(const hkl::Axe & rotation) const throw(hkl::HKLException)
    {
      if (rotation.get_type() == AXE_ROTATION)
        {
          double v1 = fmod (_consign.get_value(), 2 * constant::math::pi);
          double v2 = fmod (rotation.get_consign().get_value(), 2 * constant::math::pi);

          return ::acos(::cos(v1 - v2));
        }
      else
        HKLEXCEPTION("Cannot compute the distance between 2 different type of Axes", "Check the constitancy of the geometry.");
    }

    /**
     * @brief Applie to a hkl::Quaternion, the Rotation.
     * @return The modified hkl::Quaternion
     */
    hkl::Quaternion & Rotation::apply(hkl::Quaternion & q)
    {
      q *= _quaternion;
      return q;
    }

    /**
     * @brief Applie to a hkl::Quaternion, the Rotation.
     * @return The modified hkl::Quaternion
     */
    hkl::Quaternion & Rotation::apply_consign(hkl::Quaternion & q)
    {
      q *= _quaternion_consign;
      return q;
    }

    /*!
     * \brief print the Rotation into a flux
     * \param flux The stream to print into.
     */
    std::ostream & Rotation::printToStream(std::ostream & flux) const
      {
        flux << " Rotation : ";
        Axe::printToStream (flux);
        flux << " " << _axe;
        return flux;
      }

    /*!
     * \brief Save the Rotation into a stream.
     * \param flux the stream to save the Rotation into.
     * \return The stream with the Rotation.
     */
    std::ostream & Rotation::toStream(std::ostream & flux) const
      {
        Axe::toStream(flux);
        _axe.toStream(flux);
        _quaternion.toStream(flux);
        _quaternion_consign.toStream(flux);

        return flux;
      }

    /*!
     * \brief Restore a Rotation from a stream.
     * \param flux The stream containing the Rotation to restore.
     * @todo call update_observers or not ?
     */
    std::istream & Rotation::fromStream(std::istream & flux)
    {
      Axe::fromStream(flux);
      _axe.fromStream(flux);
      _quaternion.fromStream(flux);
      _quaternion_consign.fromStream(flux);

      return flux;
    }


  } // namespace hkl::axe

} // namespace hkl
