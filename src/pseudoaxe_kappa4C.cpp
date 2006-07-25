#include "pseudoaxe_kappa4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace kappa4C {
            namespace vertical {

                /*******************/
                /* OMEGA PSEUDOAXE */
                /*******************/
                Omega::Omega(void) :
                  PseudoAxe<geometry::kappa4C::Vertical>()
                {
                  set_name ("omega");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                }

                Omega::~Omega(void)
                  {}

                void
                Omega::initialize(geometry::kappa4C::Vertical const & geometry) throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        m_wasInitialized = true;
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                bool
                Omega::get_isValid(geometry::kappa4C::Vertical const & geometry) const
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Omega::get_value(geometry::kappa4C::Vertical const & geometry) const throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double const & komega = geometry.m_komega.get_value();
                        double const & kappa = geometry.m_kappa.get_value();

                        return komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                void
                Omega::set_value(geometry::kappa4C::Vertical & geometry,
                                 double const & value) const throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double komega;
                        double kappa = geometry.m_kappa.get_value();
                        double kphi = geometry.m_kphi.get_value();

                        double const & omega = value;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                        double p = asin(tan(chi/2.)/tan(alpha));
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                        kphi = phi + p + constant::math::pi/2.;

                        geometry.m_komega.set_value(komega);
                        geometry.m_kappa.set_value(kappa);
                        geometry.m_kphi.set_value(kphi);
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                /*****************/
                /* CHI PSEUDOAXE */
                /*****************/
                Chi::Chi(void) :
                  PseudoAxe<geometry::kappa4C::Vertical>()
                {
                  set_name ("chi");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                }

                Chi::~Chi(void)
                  {}

                void
                Chi::initialize(geometry::kappa4C::Vertical const & geometry) throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        m_wasInitialized = true;
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                bool
                Chi::get_isValid(geometry::kappa4C::Vertical const & geometry) const
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Chi::get_value(geometry::kappa4C::Vertical const & geometry) const throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double const & kappa = geometry.m_kappa.get_value();
                        return -2 * asin(sin(kappa/2.) * sin(alpha));
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                void
                Chi::set_value(geometry::kappa4C::Vertical & geometry,
                               double const & value) const throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        if (fabs(value) <= 2 * alpha)
                          {

                            double komega = geometry.m_komega.get_value();
                            double kappa = geometry.m_kappa.get_value();
                            double kphi = geometry.m_kphi.get_value();

                            double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                            double const & chi = value;
                            double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                            double p = asin(tan(chi/2.)/tan(alpha));
                            komega = omega + p - constant::math::pi/2.;
                            kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                            kphi = phi + p + constant::math::pi/2.;

                            geometry.m_komega.set_value(komega);
                            geometry.m_kappa.set_value(kappa);
                            geometry.m_kphi.set_value(kphi);
                          }
                        else
                          {
                            ostringstream reason;
                            reason << "Unreachable \"" << get_name() << "\" PseudoAxe value"; 
                            HKLEXCEPTION(reason.str(),
                                         "|chi| <= 2 * alpha");
                          }
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "Cannot set the \"" << get_name() << "\" PseudoAxe value with a null alpha";
                        HKLEXCEPTION(reason.str(),
                                     "please set a correct alpha.");
                      }
                  }

                /*****************/
                /* PHI PSEUDOAXE */
                /*****************/
                Phi::Phi(void) :
                  PseudoAxe<geometry::kappa4C::Vertical>()
                {
                  set_name ("phi");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                }

                Phi::~Phi(void)
                  {}

                void
                Phi::initialize(geometry::kappa4C::Vertical const & geometry) throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        m_wasInitialized = true;
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                bool
                Phi::get_isValid(geometry::kappa4C::Vertical const & geometry) const
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Phi::get_value(geometry::kappa4C::Vertical const & geometry) const throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double const & kappa = geometry.m_kappa.get_value();
                        double const & kphi = geometry.m_kphi.get_value();

                        return kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

                void
                Phi::set_value(geometry::kappa4C::Vertical & geometry,
                               double const & value) const throw (HKLException)
                  {
                    double const & alpha = geometry.get_alpha();
                    if (fabs(alpha) > constant::math::epsilon_0)
                      {
                        double komega = geometry.m_komega.get_value();
                        double kappa = geometry.m_kappa.get_value();
                        double kphi;

                        double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double const & phi = value;

                        double p = asin(tan(chi/2.)/tan(alpha));
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                        kphi = phi + p + constant::math::pi/2.;

                        geometry.m_komega.set_value(komega);
                        geometry.m_kappa.set_value(kappa);
                        geometry.m_kphi.set_value(kphi);
                      }
                    else
                        HKLEXCEPTION("the alpha angle is not set properly.",
                                     "please set alpha.");
                  }

            } // namespace vertical
        } // namespace kappa4C
    } // namespace pseudoAxe
} // namespace hkl
