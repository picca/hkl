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
#ifndef _TWOC_VERTICAL_DIFFRACTOMETER_H
#define _TWOC_VERTICAL_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "twoC_vertical_geometry.h"

namespace hkl
  {
  namespace twoC
    {
    namespace vertical
      {
      namespace mode
        {
        class Symetric;
      }
    }
  }
}
namespace hkl
  {
  namespace twoC
    {
    namespace vertical
      {
      namespace mode
        {
        class Fix_Incidence;
      }
    }
  }
}
namespace hkl
  {
  namespace twoC
    {
    namespace vertical
      {
      namespace pseudoAxeEngine
        {
        class Th2th;
      }
    }
  }
}
namespace hkl
  {
  namespace twoC
    {
    namespace vertical
      {
      namespace pseudoAxeEngine
        {
        class Q2th;
      }
    }
  }
}
namespace hkl
  {
  namespace twoC
    {
    namespace vertical
      {
      namespace pseudoAxeEngine
        {
        class Q;
      }
    }
  }
}

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      class Diffractometer : public hkl::DiffractometerTemp<hkl::twoC::vertical::Geometry>
        {
        public:
          Diffractometer();

          virtual ~Diffractometer();

        };

    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
#endif
