#ifndef _ANGLECONFIGURATION_EULERIAN6C_H_
#define _ANGLECONFIGURATION_EULERIAN6C_H_

#include "angleconfiguration.h"

namespace hkl {
  namespace angleConfiguration {

    /**
     * \brief Store the current angle configuration according to the type of diffractometer.
     * 
     * This class will be derived depending of the geometry of the diffractometers.
     * this class is an "abstraite" class.
     */
    class Eulerian6C : public AngleConfiguration
    {
      public:

        /**
         * \brief The default constructor
         */
        Eulerian6C();

        /**
         * \brief The destructor
         */
        virtual ~Eulerian6C();
    };

  } // namespace angleConfiguration

} // namespace hkl

#endif // _ANGLECONFIGURATION_EULERIAN6C_H_
