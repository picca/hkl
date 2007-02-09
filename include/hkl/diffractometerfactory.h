#ifndef _DIFFRACTOMETERFACTORY_H_
#define _DIFFRACTOMETERFACTORY_H_

#include "diffractometer.h"
#include "enums.h"

using namespace std;

namespace hkl
  {

  class DiffractometerFactory
    {
    public:

      /**
       * @brief The default constructor
       */
      DiffractometerFactory(void);

      /**
      * @brief The default destructor.
      */
      virtual ~DiffractometerFactory(void);

      /**
       * @brief Create a new reflection.
       * @param type The type of the diffractometer to create.
       * @param param A parameter use to build the diffractometer.
       * @return The created Diffractometer.
       *
       * This parameter has no effect for an Eulerian diffractometer.
       * But correspond to the alpha angle of the Kappa Geometry for the Kappa diffractometers.
       */
      Diffractometer * create(DiffractometerType const & type, double param) throw (HKLException);
    };

} // namespace hkl

#endif // _DIFFRACTOLMETERFACTORY_H_
