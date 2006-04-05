#include "mode_eulerian4C.h"
#include "convenience.h"

namespace hkl {
    namespace mode {

        Eulerian4C::Eulerian4C(void) {}

        Eulerian4C::~Eulerian4C(void) {}

        namespace eulerian4C {

            /******************/
            /* BISSECTOR MODE */
            /******************/
            Bissector::Bissector(void)
              {
                set_name("Bissector");
                set_description("Omega = 2theta / 2. \n there is no parameters for this mode.");
              }

            Bissector::~Bissector(void) {}

            void
            Bissector::computeAngles(double h, double k, double l,
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
                                       "Mode_Eulerian4C_bissector::computeAngles");
                }

                // Calcule de Omega
                double omega = theta;

                // Calcule de Chi
                double s_chi = hphi[1];
                double c_chi = hphi[0]*hphi[0]+hphi[2]*hphi[2];
                if (c_chi < 0.)
                    throw HKLException("Unobtainable reflection.",
                                       "Change h k l values",
                                       "Mode_Eulerian4C_Bissector::computeAngles");
                else
                    c_chi = sqrt(c_chi);
                double chi = convenience::atan2(s_chi, c_chi);

                // Calcule de Phi
                double s_phi = hphi[0];
                double c_phi = hphi[2];
                double phi = convenience::atan2(s_phi, c_phi);

                geometry.get_axe("omega").set_value(omega);
                geometry.get_axe("chi").set_value(chi);
                geometry.get_axe("phi").set_value(phi);
                geometry.get_axe("2theta").set_value(2.*theta);
              }

            /***************/
            /* DELTA THETA */
            /***************/

            Delta_Theta::Delta_Theta(void)
              {
                set_name("Delta Theta");
                set_description("Omega = theta + dtheta \n dtheta is the only one parameter of this mode.");
                addParameter("delta theta");
              }

            Delta_Theta::~Delta_Theta(void) {}

            void
            Delta_Theta::computeAngles(double h, double k, double l,
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
                                       "Mode_Eulerian4C_Delta_Theta::computeAngles");
                }

                // Calcule de Omega
                // By definition in 4C omega constant mode.
                double dtheta = getParameterValue("delta theta");
                double omega = theta + dtheta;

                // Calcule de Chi
                double s_chi = hphi[1];
                double c_chi = hphi[0]*hphi[0]-hphi[1]*hphi[1]*tan(dtheta)*tan(dtheta)+hphi[2]*hphi[2];
                if (c_chi < 0.)
                    throw HKLException("Unobtainable reflection.",
                                       "Change h k l values",
                                       "Mode_Eulerian4C_Delta_Theta::computeAngles");
                else
                    c_chi = sqrt(c_chi) * cos(dtheta);
                double chi = convenience::atan2(s_chi, c_chi);

                // Calcule de Phi
                double s_phi = hphi[0]*cos(dtheta)*cos(chi)-hphi[2]*sin(dtheta);
                double c_phi = hphi[2]*cos(dtheta)*cos(chi)+hphi[0]*sin(dtheta);
                double phi = convenience::atan2(s_phi, c_phi);

                geometry.get_axe("omega").set_value(omega);
                geometry.get_axe("chi").set_value(chi);
                geometry.get_axe("phi").set_value(phi);
                geometry.get_axe("2theta").set_value(2.*theta);
              }

            /******************/
            /* CONSTANT OMEGA */
            /******************/

            Constant_Omega::Constant_Omega(void)
              {
                set_name("Constant Omega");
                set_description("Omega = Constante \n  omega is the only one parameter of this mode.");
                addParameter("omega");
              }

            Constant_Omega::~Constant_Omega(void) {}

            void
            Constant_Omega::computeAngles(double h, double k, double l,
                                          smatrix const & UB, Geometry & geometry) const throw (HKLException)
              {
                // calcule de Theta
                double theta;
                svector hphi = UB * svector(h,k,l);
                try {
                    double lambda = geometry.get_source().get_waveLength();
                    theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
                } catch (const HKLException &) {
                    throw HKLException("sine bigger than 1.",
                                       "Maybe error in UB matrix",
                                       "Mode_Eulerian4C_Constant_Omega::computeAngles");
                }

                // La définition de omega dans ce mode.
                double omega = getParameterValue("omega");

                // calcule de Chi.
                double s_chi = hphi[1];
                double c_chi = (hphi[0]*hphi[0] + hphi[2]*hphi[2])*cos(omega-theta)*cos(omega-theta)-hphi[1]*hphi[1]*sin(omega-theta)*sin(omega-theta);
                if (c_chi < 0.)
                    throw HKLException("Unobtainable reflection.",
                                       "Change h k l values",
                                       "Mode_Eulerian4C_Constant_Omega::computeAngles");
                else
                    c_chi = sqrt(c_chi);
                double chi = convenience::atan2(s_chi, c_chi);

                // Calcule de Phi
                double s_phi = hphi[0]*cos(chi)*cos(omega - theta) - hphi[2]*sin(omega - theta);
                double c_phi = hphi[0]*sin(omega - theta) + hphi[2]*cos(chi)*cos(omega - theta);
                double phi = convenience::atan2(s_phi, c_phi);

                geometry.get_axe("omega").set_value(omega);
                geometry.get_axe("chi").set_value(chi);
                geometry.get_axe("phi").set_value(phi);
                geometry.get_axe("2theta").set_value(2.*theta);
              }

            /****************/
            /* CONSTANT CHI */
            /****************/

            Constant_Chi::Constant_Chi(void)
              {
                set_name("Constant Chi");
                set_description("chi = Constante \n  chi is the only one parameter of this mode.");
                addParameter("chi");
              }

            Constant_Chi::~Constant_Chi(void) {}

            void
            Constant_Chi::computeAngles(double h, double k, double l,
                                        smatrix const & UB, Geometry & geometry) const throw (HKLException)
              {
                // calcule de hphi
                svector hphi = UB * svector(h,k,l);

                // calcule de Theta
                double theta;
                try {
                    double lambda = geometry.get_source().get_waveLength();
                    theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
                } catch (const HKLException &) {
                    throw HKLException("sine bigger than 1.",
                                       "Maybe error in UB matrix",
                                       "Mode_Eulerian4C_Constant_Chi::computeAngles");
                }

                // La définition de chi dans ce mode.
                double chi = getParameterValue("chi");
                //! \todo traiter le cas C=0;

                // calcule de Omega.
                double s_omega_theta = (hphi[0]*hphi[0] + hphi[2]*hphi[2])*sin(chi)*sin(chi) - hphi[1]*hphi[1]*cos(chi)*cos(chi);
                double c_omega_theta = hphi[1];
                if (s_omega_theta < 0.)
                    throw HKLException("Unobtainable reflection.",
                                       "Change h k l values",
                                       "Mode_Eulerian4C_Constant_Chi::computeAngles");
                else
                    s_omega_theta = sqrt(s_omega_theta);
                double omega = convenience::atan2(s_omega_theta, c_omega_theta) + theta;

                // Calcule de Phi
                double s_phi = hphi[0]*cos(chi)*cos(omega - theta) - hphi[2]*sin(omega - theta);
                double c_phi = hphi[0]*sin(omega - theta) + hphi[2]*cos(chi)*cos(omega - theta);
                double phi = convenience::atan2(s_phi, c_phi);

                geometry.get_axe("omega").set_value(omega);
                geometry.get_axe("chi").set_value(chi);
                geometry.get_axe("phi").set_value(phi);
                geometry.get_axe("2theta").set_value(2.*theta);
              }

            /****************/
            /* CONSTANT PHI */
            /****************/

            Constant_Phi::Constant_Phi(void)
              {
                set_name("Constant Phi");
                set_description("phi = Constante \n  phi is the only one parameter of this mode.");
                addParameter("phi");
              }

            Constant_Phi::~Constant_Phi(void) {}

            void
            Constant_Phi::computeAngles(double h, double k, double l,
                                        smatrix const & UB, Geometry & geometry) const throw (HKLException)
              {
                // calcule de Theta
                double theta;
                svector hphi = UB * svector(h,k,l);
                try {
                    double lambda = geometry.get_source().get_waveLength();
                    theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
                } catch (const HKLException &) {
                    throw HKLException("sine bigger than 1.",
                                       "Maybe error in UB matrix",
                                       "Mode_Eulerian4C_Constant_Chi::computeAngles");
                }

                // La définition de chi dans ce mode.
                double phi = getParameterValue("phi");

                // calcule de Omega.
                double s_omega_theta = hphi[0]*cos(phi)-hphi[2]*sin(phi);
                double c_omega_theta = hphi[0]*hphi[0]*sin(phi)*sin(phi)+hphi[1]*hphi[1]+hphi[2]*hphi[2]*cos(phi)*cos(phi)+hphi[0]*hphi[2]*cos(phi)*sin(phi);
                if (c_omega_theta < 0.)
                    throw HKLException("Unobtainable reflection.",
                                       "Change h k l values",
                                       "Mode_Eulerian4C_Constant_Chi::computeAngles");
                else
                    c_omega_theta = sqrt(c_omega_theta);
                double omega = convenience::atan2(s_omega_theta, c_omega_theta) + theta;

                // Calcule de Chi
                double s_chi = hphi[1];
                double c_chi = hphi[0]*sin(phi) + hphi[2]*cos(phi);
                double chi = convenience::atan2(s_chi, c_chi);

                geometry.get_axe("omega").set_value(omega);
                geometry.get_axe("chi").set_value(chi);
                geometry.get_axe("phi").set_value(phi);
                geometry.get_axe("2theta").set_value(2.*theta);
              }

        } // namespace eulerian4C
    } // namespace mode
} // namespace hkl
