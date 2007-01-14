#ifndef _PSEUDOAXEENGINE_KAPPA4C_H_
#define _PSEUDOAXEENGINE_KAPPA4C_H_

#include "pseudoaxeengine.h"
#include "geometry_kappa4C.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace kappa4C
      {
      namespace vertical
        {

        /**
         * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
         */
        class Eulerians : public PseudoAxeEngineTemp<geometry::kappa4C::Vertical>
          {
          public:

            Eulerians(geometry::kappa4C::Vertical &, vector<string> const & names); //!< Default constructor.

            virtual ~Eulerians(void); //!< Default destructor.

            void initialize(void) throw (HKLException);

            void update(void) throw (HKLException);

            void set(void) throw (HKLException);

          private:
            double _alpha; //!< The alpha value.
            Axe * _komega; //!< the komega Axe.
            Axe * _kappa; //!< the kappa Axe.
            Axe * _kphi; //!< the kphi Axe.
            Parameter * _solution; //!< Switch between the two possible convert-solution.

            Range _omega_r;
            Range _omega_w;
            Range _chi_r;
            Range _chi_w;
            Range _phi_r;
            Range _phi_w;

            PseudoMultiAxe * _omega;
            PseudoMultiAxe * _chi;
            PseudoMultiAxe * _phi;
          };

      } // namespace vertical
    } // namespace kappa4C
  } // namespace pseudoMultiAxeEngine
} // namespace hkl

#endif // _PSEUDOMULYIAXEENGINE_EULERIAN4C_H_
