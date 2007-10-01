#include <assert.h>

#include "config.h"
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
    Rotation::Rotation(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, hkl_svector const * axe) :
        hkl::Axe(name, description , min, current, max)
    {
      //check the validity of input parameters
      assert(::hkl_svector_norm2(axe) > HKL_EPSILON);

      _axe = *axe;
      ::hkl_svector_normalize(&_axe);

      ::hkl_quaternion_from_angle_and_axe(&_quaternion, current.get_value(), &_axe);
      _quaternion_consign = _quaternion;
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
      ::hkl_quaternion_from_angle_and_axe(&_quaternion, value.get_value(), &_axe);
      // call the Axe::set_current to updates all related PseudoAxes
      Axe::set_current(value);
    }

    /**
     * @brief Set the read part of the Rotation
     */
    void Rotation::set_consign(const hkl::Value & value)
    {
      // update the _quaternion_consign
      ::hkl_quaternion_from_angle_and_axe(&_quaternion_consign, value.get_value(), &_axe);
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
               && ::hkl_svector_cmp(&_axe, &(rotation._axe))
               && ::hkl_quaternion_cmp(&_quaternion,&(rotation._quaternion))
               && ::hkl_quaternion_cmp(&_quaternion_consign,&(rotation._quaternion_consign));
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
          double v1 = fmod (_current.get_value(), 2 * M_PI);
          double v2 = fmod (rotation.get_current().get_value(), 2 * M_PI);

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
          double v1 = fmod (_consign.get_value(), 2 * M_PI);
          double v2 = fmod (rotation.get_consign().get_value(), 2 * M_PI);

          return ::acos(::cos(v1 - v2));
        }
      else
        HKLEXCEPTION("Cannot compute the distance between 2 different type of Axes", "Check the constitancy of the geometry.");
    }

    /**
     * @brief Applie to a hkl::Quaternion, the Rotation.
     * @return The modified hkl::Quaternion
     */
    hkl_quaternion * Rotation::apply(hkl_quaternion * q)
    {
      ::hkl_quaternion_times_quaternion(q, &_quaternion);
      return q;
    }

    /**
     * @brief Applie to a hkl::Quaternion, the Rotation.
     * @return The modified hkl::Quaternion
     */
    hkl_quaternion * Rotation::apply_consign(hkl_quaternion * q)
    {
      ::hkl_quaternion_times_quaternion(q, &_quaternion_consign);
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
        ::hkl_svector_fprintf(stdout, &_axe);
        return flux;
      }

  } // namespace hkl::axe

} // namespace hkl
