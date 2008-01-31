/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _EULERIAN6C_GEOMETRY_H
#define _EULERIAN6C_GEOMETRY_H


#include "geometry.h"
#include "axe_rotation.h"
#include "HKLException.h"

namespace hkl
  {
  namespace twoC
    {
    namespace vertical
      {
      class Geometry;
    }
  }
}
namespace hkl
  {
  namespace eulerian4C
    {
    namespace vertical
      {
      class Geometry;
    }
  }
}
namespace hkl
  {
  namespace kappa4C
    {
    namespace vertical
      {
      class Geometry;
    }
  }
}
namespace hkl
  {
  namespace kappa6C
    {
    class Geometry;
  }
}

namespace hkl
  {

  namespace eulerian6C
    {

    class Geometry : public hkl::Geometry
      {
      protected:
        hkl::axe::Rotation * _mu;

        hkl::axe::Rotation * _omega;

        hkl::axe::Rotation * _chi;

        hkl::axe::Rotation * _phi;

        hkl::axe::Rotation * _gamma;

        hkl::axe::Rotation * _delta;


      public:
        /**
         *  @brief Default constructor
         */
        Geometry();

        /**
         *  @brief Another constructor.
         *  @param mu the first angle value.
         *  @param omega the second angle value.
         *  @param chi the third angle value.
         *  @param phi the fourth angle value.
         *  @param gamma the fifth angle value.
         *  @param delta the sixth angle value.
         */
        Geometry(double mu, double omega, double chi, double phi, double gamma, double delta);

        virtual ~Geometry();

        /**
         * @brief Copy Constructor.
         */
        Geometry(const Geometry & geometry);

        /**
         * @brief Get the _mu Axe.
         * @return A pointer on the _mu Axe.
         */
        hkl::axe::Rotation * mu();

        /**
         * @brief Get the _omega Axe.
         * @return A pointer on the _omega Axe.
         */
        hkl::axe::Rotation * omega();

        /**
         * @brief Get the _chi Axe.
         * @return A pointer on the _chi Axe.
         */
        hkl::axe::Rotation * chi();

        /**
         * @brief Get the _phi Axe.
         * @return A pointer on the _phi Axe.
         */
        hkl::axe::Rotation * phi();

        /**
         * @brief Get the _gamma Axe.
         * @return A pointer on the _gamma Axe.
         */
        hkl::axe::Rotation * gamma();

        /**
         * @brief Get the _delta Axe.
         * @return A pointer on the _delta Axe.
         */
        hkl::axe::Rotation * delta();

        /**
         * @brief Get the _mu Axe.
         * @return A pointer on the _mu Axe.
         */
        const hkl::axe::Rotation * mu() const;

        /**
         * @brief Get the _omega Axe.
         * @return A pointer on the _omega Axe.
         */
        const hkl::axe::Rotation * omega() const;

        /**
         * @brief Get the _chi Axe.
         * @return A pointer on the _chi Axe.
         */
        const hkl::axe::Rotation * chi() const;

        /**
         * @brief Get the _phi Axe.
         * @return A pointer on the _phi Axe.
         */
        const hkl::axe::Rotation * phi() const;

        /**
         * @brief Get the _gamma Axe.
         * @return A pointer on the _gamma Axe.
         */
        const hkl::axe::Rotation * gamma() const;

        /**
         * @brief Get the _delta Axe.
         * @return A pointer on the _delta Axe.
         */
        const hkl::axe::Rotation * delta() const;

        /**
         * @brief Set the angles of the eulerian4CD::Vertical geometry.
         * @param mu The value of the "omega" Axe.
         * @param omega The value of the "chi" Axe.
         * @param chi The value of the "phi" Axe.
         * @param phi The value of the "2theta" Axe.
         * @param gamma The value of the "gamma" Axe.
         * @param delta The value of the "delta" Axe.
         */
        void set_angles(double mu, double omega, double chi, double phi, double gamma, double delta);

        /**
         * @brief Set the consign angles of the Geometry.
         * @param mu The value of the "mu" Axe.
         * @param omega The value of the "omega" Axe.
         * @param chi The value of the "chi" Axe.
         * @param phi The value of the "phi" Axe.
         * @param gamma The value of the "gamma" Axe.
         * @param delta The value of the "delta" Axe.
         */
        void set_angles_consign(double mu, double omega, double chi, double phi, double gamma, double delta);

        /**
         * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
         * @param geometry The hkl::twoC::vertical::Geometry.
         * @param strict false or true if we must not care of the strictness of the conversion.
         * @throw HKLException
         */
        void setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

        /**
         * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
         * @param geometry The hkl::eulerian4C::vertical::Geometry.
         * @param strict false or true if we must not care of the strictness of the conversion.
         * @throw HKLException
         */
        void setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

        /**
         * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
         * @param geometry The hkl::kappa4C::vertical::Geometry.
         * @param strict false or true if we must not care of the strictness of the conversion.
         * @throw HKLException
         */
        void setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

        /**
         * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
         * @param geometry The hkl::kappa6C::Geometry.
         * @param strict false or true if we must not care of the strictness of the conversion.
         * @throw HKLException
         */
        void setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

      };

  } // namespace hkl::eulerian6C

} // namespace hkl
#endif
