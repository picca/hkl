#ifndef _PSEUDOMULTIAXEENGINE_KAPPA4C_H_
#define _PSEUDOMULYIAXEENGINE_KAPPA4C_H_

#include "pseudomultiaxeengine.h"
#include "geometry_kappa4C.h"
#include "pseudoaxe_twoC.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl
  {
  namespace pseudoMultiAxeEngine
    {
    namespace kappa4C
      {
      namespace vertical
        {

        /**
         * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
         */
        class Eulerians : public PseudoMultiAxeEngineTemp<geometry::kappa4C::Vertical>
          {
          public:

            Eulerians(geometry::kappa4C::Vertical &); //!< Default constructor.

            virtual ~Eulerians(void); //!< Default destructor.

            void update(void) throw (HKLException);

            void set(void) throw (HKLException);

          private:
            double _alpha; //!< The alpha value.
            Axe * _komega; //!< the komega Axe.
            Axe * _kappa; //!< the kappa Axe.
            Axe * _kphi; //!< the kphi Axe.
            Parameter * _solution; //!< Switch between the two possible convert-solution.

            PseudoMultiAxe * _omega;
            PseudoMultiAxe * _chi;
            PseudoMultiAxe * _phi;
          };

      } // namespace vertical
    } // namespace kappa4C
  } // namespace pseudoMultiAxeEngine
} // namespace hkl

#endif // _PSEUDOMULYIAXEENGINE_EULERIAN4C_H_
