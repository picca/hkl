#include "mode_twoC.h"
#include "convenience.h"

namespace hkl
  {
  namespace mode
    {
    namespace twoC
      {
      namespace vertical
        {

        /*****************/
        /* SYMETRIC MODE */
        /*****************/
        Symetric::Symetric(MyString const & name, MyString const & description,
                           geometry::twoC::Vertical & geometry) :
            ModeTemp<geometry::twoC::Vertical>(name, description , geometry)
        {}

        Symetric::~Symetric(void)
        {}

        void
        Symetric::computeAngles(Value const & h, Value const & k, Value const & l,
                                smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              _geometry.omega()->set_current(theta);
              _geometry.tth()->set_current(2.*theta);
            }
        }

        /*****************/
        /* FIX INCIDENCE */
        /*****************/

        Fix_Incidence::Fix_Incidence(MyString const & name, MyString const & description,
                                     geometry::twoC::Vertical & geometry) :
            ModeTemp<geometry::twoC::Vertical>(name, description, geometry)
        {}

        Fix_Incidence::~Fix_Incidence(void)
        {}

        void
        Fix_Incidence::computeAngles(Value const & h, Value const & k, Value const & l,
                                     smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              _geometry.tth()->set_current(2.*theta);
            }
        }

      } // namespace vertical
    } // namespace twoC
  } // namespace mode
} // namespace hkl
