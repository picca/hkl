#ifndef _ANGLECONFIGURATION_KAPPA4C_H_
#define _ANGLECONFIGURATION_KAPPA4C_H_

#include "angleconfiguration.h"

namespace hkl {
  namespace angleConfiguration {

    /**
     * \brief An %AngleConfiguration for a the kappa 4 circle soleil generic diffractometer.
     */
    class Kappa4C : public AngleConfiguration
    {
      public:

        /**
         * \brief The default constructor
         */
        Kappa4C();

        /**
         * \brief The destructor
         */
        virtual ~Kappa4C();
    };

  } // namespace angleConfiguration
} // namespace hkl

#endif // _ANGLECONFIGURATION_KAPPA4C_H_
