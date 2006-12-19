#ifndef _MODE_EULERIAN4C_H_
#define _MODE_EULERIAN4C_H_

#include "derivedmode.h"
#include "geometry_eulerian4C.h"

namespace hkl
  {
  namespace mode
    {
    namespace eulerian4C
      {
      namespace vertical
        {

        /*!
         * The eulerian 4-circle diffractometer in bissector mode.
         */
        class Bissector : public ModeTemp<geometry::eulerian4C::Vertical>
          {
          public:

            Bissector(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry); //!< Default constructor.

            virtual ~Bissector(void); //!< Default Destructor.

            void computeAngles(Value const & h, Value const & k, Value const& l,
                               smatrix const & UB) const throw (HKLException);
          };

        /*!
         * \brief The Delta Theta computational mode for the Eulerian4C diffractometer.
         *
         * In this mode \f$ \omega = \theta + d\theta \f$
         */
        class Delta_Theta : public ModeTemp<geometry::eulerian4C::Vertical>
          {
          public:

            Delta_Theta(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry); //!< Default contructor.

            virtual ~Delta_Theta(void); //!< Default destructor.

            void computeAngles(Value const& h, Value const& k, Value const& l,
                               smatrix const & UB) const throw (HKLException);
          protected:
            Parameter * _dtheta; //!< The dtheta Parameter.
          };

        /*!
         * The eulerian 4-circle diffractometer in constant omega mode.
         *
         * The omega Axe is constant for this calculation.
         */
        class Constant_Omega : public ModeTemp<geometry::eulerian4C::Vertical>
          {
          public:

            Constant_Omega(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry); //!<! Default constructor.

            virtual ~Constant_Omega(void); //!< Default destructor.

            void computeAngles(Value const& h, Value const& k, Value const& l,
                               smatrix const & UB) const throw (HKLException);
          protected:
            Parameter * _omega; //!< The omega Parameter.
          };

        /*!
         * The eulerian 4-circle diffractometer in constant chi mode.
         *
         * The chi Axe is constant for this calculation.
         */
        class Constant_Chi : public ModeTemp<geometry::eulerian4C::Vertical>
          {
          public:

            Constant_Chi(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry); //!< default constructor.

            virtual ~Constant_Chi(void); //!< Default destructor.

            void computeAngles(Value const & h, Value const & k, Value const & l,
                               smatrix const & UB) const throw (HKLException);
          protected:
            Parameter * _chi; //!< The chi Parameter.
          };

        /*!
         * The eulerian 4-circle diffractometer in constant phi mode.
         *
         * The phi Axe is constant for this calculation.
         */
        class Constant_Phi : public ModeTemp<geometry::eulerian4C::Vertical>
          {
          public:

            Constant_Phi(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry); //!< The default constructor.

            virtual ~Constant_Phi(void); //!< The destructor.

            void computeAngles(Value const & h, Value const & k, Value const & l,
                               smatrix const & UB) const throw (HKLException);
          protected:
            Parameter * _phi; //!< The phi Parameter.
          };

      } // namespace vertical
    } // namespace eulerian4C
  } // namespace mode
} // namespace hkl

#endif // _MODE_EULERIAN4C_H_
