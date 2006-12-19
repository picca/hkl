#ifndef _MODE_TWOC_H
#define _MODE_TWOC_H

#include "mode.h"
#include "geometry_twoC.h"

namespace hkl
  {
  namespace mode
    {
    namespace twoC
      {
      namespace vertical
        {

        /*!
         * The Symetric mode of the vertical twoC Geometry.
         *
         * "omega" = \f$\theta\f$ and "2theta" = \f$2\theta\f$.
         */
        class Symetric : public ModeTemp<geometry::twoC::Vertical>
          {
          public:

            Symetric(MyString const & name, MyString const & description,
                     geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Symetric(void); //!< Default Destructor.

            void computeAngles(Value const & h, Value const & k, Value const & l,
                               smatrix const & UB) const throw (HKLException);
          };

        /*!
         * \brief The Delta Theta computational mode for the Eulerian4C diffractometer.
         *
         * "omega" = free, "2theta" = \f$2\theta\f$
         */
        class Fix_Incidence : public ModeTemp<geometry::twoC::Vertical>
          {
          public:

            Fix_Incidence(MyString const & name, MyString const & description,
                          geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Fix_Incidence(void); //!< Default destructor.

            void computeAngles(Value const & h, Value const & k, Value const & l,
                               smatrix const & UB) const throw (HKLException);

          };

      } // namespace vertical
    } // namespace twoC
  } // namespace mode
} // namespace hkl

#endif // _MODE_TWOC_H
