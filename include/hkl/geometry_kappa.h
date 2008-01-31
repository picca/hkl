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
#ifndef _GEOMETRY_KAPPA_H
#define _GEOMETRY_KAPPA_H


#include "geometry.h"
#include <string>
#include <ostream>
#include <istream>

namespace hkl
  {

  namespace geometry
    {

    class Kappa : public hkl::Geometry
      {
      protected:
        double _alpha;

        Kappa(const std::string & name, const std::string & description, double alpha);


      public:
        virtual ~Kappa();

        inline const double get_alpha() const;

        /**
         * @brief print the Kappa into a flux
         * @param flux The stream to print into.
         * @return The modified flux.
         */
        std::ostream & printToStream(std::ostream & flux) const;

        /**
         * @brief print on a stream the content of the Kappa
         * @param flux the ostream to modify.
         * @return the modified ostream
         */
        std::ostream & toStream(std::ostream & flux) const;

        /**
         * @brief restore the content of the Kappa from an istream
         * @param flux the istream.
         * @return the modified istream.
         * @todo problem of security here.
         */
        std::istream & fromStream(std::istream & flux);

      };
    inline const double Kappa::get_alpha() const
      {
        return _alpha;
      }


  } // namespace hkl::geometry

} // namespace hkl
#endif
