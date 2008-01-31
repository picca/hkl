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

#include "twoC_vertical_mode.h"
#include "value.h"
#include "svector.h"

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      namespace mode
        {

        Symetric::Symetric(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry) :
            ModeTemp<hkl::twoC::vertical::Geometry>(name, description, geometry)
        {
        }

        Symetric::~Symetric()
        {
        }

        /**
         * @brief The main function to get a sample of angles from (h,k,l).
         * @param h The scaterring vector first coordinate.
         * @param k The scaterring vector second coordinate.
         * @param l The scaterring vector third coordinate.
         * @param UB The product of the orientation matrix U by the crystal matrix B.
         */

        void Symetric::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const
          {
            if (this->_parametersAreOk(h, k, l, UB))
              {
                double theta;
                svector hphi;
                this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);

                _geometry.omega()->set_consign(theta);
                _geometry.tth()->set_consign(2.*theta);
              }
          }

        Fix_Incidence::Fix_Incidence(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry) :
            ModeTemp<hkl::twoC::vertical::Geometry>(name, description, geometry)
        {
        }

        Fix_Incidence::~Fix_Incidence()
        {
        }

        /**
         * @brief The main function to get a sample of angles from (h,k,l).
         * @param h The scaterring vector first coordinate.
         * @param k The scaterring vector second coordinate.
         * @param l The scaterring vector third coordinate.
         * @param UB The product of the orientation matrix U by the crystal matrix B.
         */

        void Fix_Incidence::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const
          {
            if (this->_parametersAreOk(h, k, l, UB))
              {
                double theta;
                svector hphi;
                this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);

                _geometry.tth()->set_consign(2.*theta);
              }
          }


      } // namespace hkl::twoC::vertical::mode

    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
