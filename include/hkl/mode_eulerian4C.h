#ifndef _MODE_EULERIAN4C_H_
#define _MODE_EULERIAN4C_H_

#include "mode.h"

namespace hkl {
  namespace mode {

    /*!
     * This class defines the mode for all the 4 circles Eulerian diffractometers.
     */
    class Eulerian4C : virtual public Mode
    {
      public:

        virtual ~Eulerian4C(void); //!< Default destructor

        /*!
         * \brief The main function to get a sample of angles from (h,k,l).
         * \param h The scaterring vector first element.
         * \param k The scaterring vector second element.
         * \param l The scaterring vector third element.
         * \param UB The product of the orientation matrix U by the crystal matrix B.
         * \param[out] geometry The Geometry to calculate.
         *
         * The main function to get a sample of angles from (h,k,l).
         */
        virtual void computeAngles(double h, double k, double l,
                                   smatrix const & UB,
                                   Geometry & geometry) const = 0;

      protected:

        Eulerian4C(void); //!< Default constructor - protected to make sure this class is abstract.
    };

    namespace eulerian4C {
      
      /*!
       * The eulerian 4-circle diffractometer in bissector mode.
       */
      class Bissector : public Eulerian4C
      {
        public:

          Bissector(void); //!< Default constructor.

          virtual ~Bissector(void); //!< Default Destructor.

          virtual void computeAngles(double h, double k, double l,
                                     smatrix const & UB,
                                     Geometry & geometry) const throw (HKLException);
      };

      /*!
       * \brief The Delta Theta computational mode for the Eulerian4C diffractometer.
       *
       * In this mode \f$ \omega = \theta + d\theta \f$
       */
      class Delta_Theta : public Eulerian4C
      {
        public:

          Delta_Theta(void); //!< Default contructor.

          virtual ~Delta_Theta(void); //!< Default destructor.

          void computeAngles(double h, double k, double l,
                             smatrix const & UB,
                             Geometry & geometry) const throw (HKLException);

      };

      /*!
       * The eulerian 4-circle diffractometer in constant omega mode.
       *
       * The omega Axe is constant for this calculation.
       */
      class Constant_Omega : public Eulerian4C
      {
        public:

          Constant_Omega(void); //!<! Default constructor.

          virtual ~Constant_Omega(void); //!< Default destructor.

          void computeAngles(double h, double k, double l,
                             smatrix const & UB,
                             Geometry & geometry) const throw (HKLException);
      };

      /*!
       * The eulerian 4-circle diffractometer in constant chi mode.
       *
       * The chi Axe is constant for this calculation.
       */
      class Constant_Chi : public Eulerian4C
      {
        public:

          Constant_Chi(void); //!< default constructor.

          virtual ~Constant_Chi(void); //!< Default destructor.

          void computeAngles(double h, double k, double l,
                             smatrix const & UB,
                             Geometry & geometry) const throw (HKLException);
      };

      /*!
       * The eulerian 4-circle diffractometer in constant phi mode.
       *
       * The phi Axe is constant for this calculation.
       */
      class Constant_Phi : public Eulerian4C
      {
        public:

          Constant_Phi(void); //!< The default constructor.

          virtual ~Constant_Phi(void); //!< The destructor.

          void computeAngles(double h, double k, double l,
                             smatrix const & UB,
                             Geometry & geometry) const throw (HKLException);
      };

    } // namespace eulerian4C
  } // namespace mode
} // namespace hkl

#endif // _MODE_EULERIAN4C_H_
