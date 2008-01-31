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
#ifndef _TWOC_VERTICAL_MODE_H
#define _TWOC_VERTICAL_MODE_H


#include "mode.h"
#include <string>
#include "twoC_vertical_geometry.h"

namespace hkl
  {
  class Value;
}
namespace hkl
  {
  class smatrix;
}

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      namespace mode
        {

        class Symetric : public hkl::ModeTemp<hkl::twoC::vertical::Geometry>
          {
          public:
            Symetric(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry);

            virtual ~Symetric();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };
        class Fix_Incidence : public hkl::ModeTemp<hkl::twoC::vertical::Geometry>
          {
          public:
            Fix_Incidence(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry);

            virtual ~Fix_Incidence();

            /**
             * @brief The main function to get a sample of angles from (h,k,l).
             * @param h The scaterring vector first coordinate.
             * @param k The scaterring vector second coordinate.
             * @param l The scaterring vector third coordinate.
             * @param UB The product of the orientation matrix U by the crystal matrix B.
             */

            virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

          };

      } // namespace hkl::twoC::vertical::mode

    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
#endif
