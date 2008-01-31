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
#ifndef _EULERIAN4C_VERTICAL_GEOMETRY_H
#define _EULERIAN4C_VERTICAL_GEOMETRY_H


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
  namespace eulerian6C
    {
    class Geometry;
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

  namespace eulerian4C
    {

    namespace vertical
      {

      class Geometry : public hkl::Geometry
        {
        protected:
          hkl::axe::Rotation * _omega;

          hkl::axe::Rotation * _chi;

          hkl::axe::Rotation * _phi;

          hkl::axe::Rotation * _tth;


        public:
          /**
           *  @brief Default constructor
           */
          Geometry();

          /**
           *  @brief Another constructor.
           *  @param omega the first angle value.
           *  @param chi the second angle value.
           *  @param phi the third angle value.
           *  @param tth the fourth angle value.
           */
          Geometry(double omega, double chi, double phi, double tth);

          virtual ~Geometry();

          /**
           * @brief Copy Constructor.
           */
          Geometry(const Geometry & geometry);

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
           * @brief Get the _tth Axe.
           * @return A pointer on the _tth Axe.
           */
          hkl::axe::Rotation * tth();

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
           * @brief Get the _tth Axe.
           * @return A pointer on the _tth Axe.
           */
          const hkl::axe::Rotation * tth() const;

          /**
           * @brief Set the angles of the eulerian4CD::Vertical geometry.
           * @param omega The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param tth The value of the "tth" Axe.
           */
          void set_angles(double omega, double chi, double phi, double tth);

          /**
           * @brief Set the angles of the eulerian4CD::Vertical geometry.
           * @param omega The value of the "omega" Axe.
           * @param chi The value of the "chi" Axe.
           * @param phi The value of the "phi" Axe.
           * @param tth The value of the "tth" Axe.
           */
          void set_angles_consign(double omega, double chi, double phi, double tth);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::twoC::vertical::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::kappa4C::vertical::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::kappa4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::eulerian6C::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

          /**
           * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
           * @param geometry The hkl::kappa6C::Geometry.
           * @param strict false or true if we must not care of the strictness of the conversion.
           * @throw HKLException
           */
          void setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException);

        };

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
#endif
