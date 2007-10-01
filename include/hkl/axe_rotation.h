#ifndef _HKL_AXE_AXE_ROTATION_H
#define _HKL_AXE_AXE_ROTATION_H

#include "axe.h"
#include "svector.h"
#include "quaternion.h"
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class Value;
}

namespace hkl
  {

  namespace axe
    {

    /**
     * @todo All the unit tests.
     */
    class Rotation : public hkl::Axe
      {
      protected:
        hkl_svector _axe;

        hkl_quaternion _quaternion;

        hkl_quaternion _quaternion_consign;


      public:
        /**
         * @brief constructor
         * @param name The name of the Rotation.
         * @param description The description of the Rotation.
         * @param min The minimum of the Rotation.
         * @param current The current position of the Rotation.
         * @param max The maximum value of the Rotation.
         * @param axe The hkl::svector rotation axe coordinates.
         */
        Rotation(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, hkl_svector const * axe);

        virtual ~Rotation();

        inline virtual hkl::AxeType get_type() const;

        virtual Axe * clone() const;

        /**
         * @brief Set the read part of the Rotation
         */
        void set_current(const hkl::Value & value);

        /**
         * @brief Set the read part of the Rotation
         */
        void set_consign(const hkl::Value & value);

        /**
         * @brief Return the Rotation axe of rotation coordinates.
         * @return The axe coordinates as a 3 elements vector.
         */
        inline hkl_svector const * get_axe() const;

        /**
         * @return the Rotation axe read part as Quaternion.
         */
        inline hkl_quaternion const * get_quaternion() const;

        inline hkl_quaternion const * get_quaternion_consign() const;

        /**
         * \brief Are two Rotation equals ?
         * \param rotation the Rotation to compare with.
         * \return The comparison of the two Rotation.
         */
        bool operator==(const Rotation & rotation) const;

        /**
         * @brief Compute the read distance between two Rotation.
         * @param rotation The hkl::Axe to compute the distance from.
         * @return The distance between the two Rotation.
         */
        double get_distance(const hkl::Axe & rotation) const throw(hkl::HKLException);

        /**
         * @brief Compute the read distance between two Rotation.
         * @param rotation The hkl::Axe to compute the distance from.
         * @return The distance between the two Rotation.
         */
        double get_distance_consign(const hkl::Axe & rotation) const throw(hkl::HKLException);

        /**
         * @brief Applie to a hkl::Quaternion, the Rotation.
         * @return The modified hkl::Quaternion
         */
        virtual hkl_quaternion * apply(hkl_quaternion * q);

        /**
         * @brief Applie to a hkl::Quaternion, the Rotation.
         * @return The modified hkl::Quaternion
         */
        virtual hkl_quaternion * apply_consign(hkl_quaternion * q);

        /*!
         * \brief print the Rotation into a flux
         * \param flux The stream to print into.
         */
        std::ostream & printToStream(std::ostream & flux) const;
      };

    inline hkl::AxeType Rotation::get_type() const
      {
        // Bouml preserved body begin 0003DC02
        return AXE_ROTATION;
        // Bouml preserved body end 0003DC02
      }

    /**
     * @brief Return the Rotation axe of rotation coordinates.
     * @return The axe coordinates as a 3 elements vector.
     */
    inline const hkl_svector * Rotation::get_axe() const
      {
        return &_axe;
      }

    /**
     * @return the Rotation axe read part as Quaternion.
     */
    inline const hkl_quaternion * Rotation::get_quaternion() const
      {
        return &_quaternion;
      }

    inline const hkl_quaternion * Rotation::get_quaternion_consign() const
      {
        return &_quaternion_consign;
      }


  } // namespace hkl::axe

} // namespace hkl
#endif
