#include "mode_twoC.h"
#include "convenience.h"

namespace hkl {
    namespace mode {
        namespace twoC {
            namespace vertical {

                /*****************/
                /* SYMETRIC MODE */
                /*****************/
                Symetric::Symetric(void) :
                  Mode<geometry::twoC::Vertical>()
                {
                  set_name("Symetric");
                  set_description("Omega = 2theta / 2. = theta");
                }

                Symetric::~Symetric(void) {}

                void
                Symetric::computeAngles(double h, double k, double l,
                                        smatrix const & UB,
                                        geometry::twoC::Vertical & geometry) const throw (HKLException)
                  {
                    if (_parametersAreOk(h, k, l, UB, geometry))
                      {
                        double theta;
                        svector hphi;
                        _computeThetaAndHphi(h, k, l, UB, geometry, theta, hphi);

                        geometry.m_omega.set_value(theta);
                        geometry.m_tth.set_value(2.*theta);
                      }
                  }

                /*****************/
                /* FIX INCIDENCE */
                /*****************/

                Fix_Incidence::Fix_Incidence(void) :
                  Mode<geometry::twoC::Vertical>()
                {
                  set_name("Fix incidence");
                  set_description("2theta = 2 * theta, omega is free.");
                }

                Fix_Incidence::~Fix_Incidence(void) {}

                void
                Fix_Incidence::computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             geometry::twoC::Vertical & geometry) const throw (HKLException)
                  {
                    if (_parametersAreOk(h, k, l, UB, geometry))
                      {
                        double theta;
                        svector hphi;
                        _computeThetaAndHphi(h, k, l, UB, geometry, theta, hphi);

                        geometry.m_tth.set_value(2.*theta);
                      }
                  }

            } // namespace vertical
        } // namespace twoC
    } // namespace mode
} // namespace hkl
