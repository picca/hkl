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
