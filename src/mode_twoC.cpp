#include "mode_twoC.h"
#include "convenience.h"

namespace hkl {
    namespace mode {
        namespace twoC {
            namespace vertical {

                /*****************/
                /* SYMETRIC MODE */
                /*****************/
                Symetric::Symetric(void)
                  {
                    set_name("Symetric");
                    set_description("Omega = 2theta / 2. = theta");
                  }

                Symetric::~Symetric(void) {}

                void
                Symetric::computeAngles(double h, double k, double l,
                                        smatrix const & UB, Geometry & geometry) const throw (HKLException)
                  {
                    // Calcule de Theta
                    double theta;
                    svector hphi = UB * svector(h,k,l);
                    try {
                        double lambda = geometry.get_source().get_waveLength();
                        theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
                    } catch (HKLException const &) {
                        throw HKLException("Unobtainable reflection",
                                           "Please change h k l values or the energy.",
                                           "mode::twoC::Vertical::Symetric::computeAngles(double, double, double, smatrix const &, Geometry &)");
                    }
                    geometry.get_axe("omega").set_value(theta);
                    geometry.get_axe("2theta").set_value(2.*theta);
                  }

                /*****************/
                /* FIX INCIDENCE */
                /*****************/

                Fix_Incidence::Fix_Incidence(void)
                  {
                    set_name("Fix Incidence");
                    set_description("2theta = 2 * theta, omega is free.");
                  }

                Fix_Incidence::~Fix_Incidence(void) {}

                void
                Fix_Incidence::computeAngles(double h, double k, double l,
                                             smatrix const & UB, Geometry & geometry) const throw (HKLException)
                  {
                    // Calcule de Theta
                    double theta;
                    svector hphi = UB * svector(h,k,l);
                    try {
                        double lambda = geometry.get_source().get_waveLength();
                        theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
                    } catch (const HKLException &) {
                        throw HKLException("Unobtainable reflection",
                                           "Please change h k l values or the energy.",
                                           "mode::twoC::Vertical::Fix_Incidence::computeAngles(double, double, double, smatrix const &, Geometry &)");
                    }
                    geometry.get_axe("2theta").set_value(2.*theta);
                  }

            } // namespace vertical
        } // namespace twoC
    } // namespace mode
} // namespace hkl
