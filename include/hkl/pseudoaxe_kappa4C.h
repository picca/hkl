#ifndef _PSEUDOAXE_KAPPA4C_H_
#define _PSEUDOAXE_KAPPA4C_H_

#include "pseudoaxe.h"
#include "derivedpseudoaxe.h"
#include "geometry_kappa4C.h"
#include "pseudoaxe_twoC.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace kappa4C
      {
      namespace vertical
        {

        /**
         * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
         */
        class Omega : public PseudoAxeTemp<geometry::kappa4C::Vertical>
          {
          public:

            Omega(geometry::kappa4C::Vertical &); //!< Default constructor.

            virtual ~Omega(void); //!< Default destructor.

            void update(void);

            void set_current(Value const & value) throw (HKLException);

            bool isValid(void) throw (HKLException);

          private:
            double _alpha; //!< The alpha value.
            Axe * _komega; //!< the komega Axe.
            Axe * _kappa; //!< the kappa Axe.
            Axe * _kphi; //!< the kphi Axe.
            Parameter * _solution; //!< Switch between the two possible convert-solution.
          };

        /**
         * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
         */
        class Chi : public PseudoAxeTemp<geometry::kappa4C::Vertical>
          {
          public:

            Chi(geometry::kappa4C::Vertical &); //!< Default constructor.

            virtual ~Chi(void); //!< Default destructor.

            void update(void);

            void set_current(Value const & value) throw (HKLException);

            bool isValid(void) throw (HKLException);

          private:
            double _alpha; //!< The alpha value.
            Axe * _komega; //!< the komega Axe.
            Axe * _kappa; //!< the kappa Axe.
            Axe * _kphi; //!< the kphi Axe.
            Parameter * _solution; //!< Switch between the two possible convert-solution.
          };

        /**
         * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
         */
        class Phi : public PseudoAxeTemp<geometry::kappa4C::Vertical>
          {
          public:

            Phi(geometry::kappa4C::Vertical &); //!< Default constructor.

            virtual ~Phi(void); //!< Default destructor.

            void update(void);

            void set_current(Value const & value) throw (HKLException);

            bool isValid(void) throw (HKLException);

          private:
            double _alpha; //!< The alpha value.
            Axe * _komega; //!< the komega Axe.
            Axe * _kappa; //!< the kappa Axe.
            Axe * _kphi; //!< the kphi Axe.
            Parameter * _solution; //!< Switch between the two possible convert-solution.
          };

        namespace eulerian4C
          {

          typedef DerivedPseudoAxe<pseudoAxe::eulerian4C::vertical::Psi, geometry::kappa4C::Vertical> Psi; //!< DerivedPseudoAxe from eulerian4C pseudoAxes.

        } // namespace eulerian4C

        namespace twoC
          {

          typedef DerivedPseudoAxe<pseudoAxe::twoC::vertical::Th2th, geometry::kappa4C::Vertical> Th2th; //!< DerivedPseudoAxe from twoC pseudoAxes.
          typedef DerivedPseudoAxe<pseudoAxe::twoC::vertical::Q2th, geometry::kappa4C::Vertical> Q2th; //!< DerivedPseudoAxe from twoC pseudoAxes.
          typedef DerivedPseudoAxe<pseudoAxe::twoC::vertical::Q, geometry::kappa4C::Vertical> Q; //!< DerivedPseudoAxe from twoC pseudoAxes.

        } // namespace twoC
      } // namespace vertical
    } // namespace kappa4C
  } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
