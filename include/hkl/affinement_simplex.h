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
#ifndef _AFFINEMENT_SIMPLEX_H
#define _AFFINEMENT_SIMPLEX_H


#include "affinement.h"
#include "HKLException.h"
#include <valarray>
using namespace std;

namespace hkl
  {
  class FitParameterList;
}

namespace hkl
  {

  namespace affinement
    {

    class Simplex : public hkl::Affinement
      {
      public:
        Simplex();

        virtual ~Simplex();

        /**
         * @brief fit the data using the simplex method.
         * @param fitParameterList the hkl::FitParameterList to fit.
         *
         * This function modify the vertex.
         */

        virtual void fit(hkl::FitParameterList & fitParameterList) throw(hkl::HKLException);


      protected:
        void _updateParameterListFromVertex(const hkl::FitParameterList & fitParameterList, valarray<double> & parameterList);

        void _updateVertexFromParameterList(hkl::FitParameterList & fitParameterList, const valarray<double> & parameterList);

      };

  } // namespace hkl::affinement

} // namespace hkl
#endif
