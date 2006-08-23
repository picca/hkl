#include "pseudoaxe_kappa4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace kappa4C {
            namespace vertical {

                /*******************/
                /* OMEGA PSEUDOAXE */
                /*******************/
                Omega::Omega(geometry::kappa4C::Vertical & geometry) :
                  PseudoAxe<geometry::kappa4C::Vertical>(geometry)
                {
                  set_name ("omega");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                  m_initialized = true;
                  m_writable = true;
                }

                Omega::~Omega(void)
                  {}

                double
                Omega::get_min(void) const
                  {
                    double min = 0;
                    if (m_initialized)
                        min = -constant::math::pi;
                    return min;
                  }

                double
                Omega::get_max(void) const
                  {
                    double max = 0;
                    if (m_initialized)
                        max = constant::math::pi;
                    return max;
                  }

                bool
                Omega::isValid(void) throw (HKLException)
                  {
                    if (PseudoAxe<geometry::kappa4C::Vertical>::isValid())
                      {
                        try
                          {
                            m_geometry.isValid();
                          }
                        catch (HKLException &)
                          {
                            m_writable = false;
                            throw;
                          }
                      }
                    return true;
                  }

                double
                Omega::get_value(void) throw (HKLException)
                  {
                    if (Omega::isValid())
                      {
                        double const & alpha = m_geometry.get_alpha();
                        double const & komega = m_geometry.m_komega.get_value();
                        double const & kappa = m_geometry.m_kappa.get_value();

                        return komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                      }
                  }

                void
                Omega::set_value(double const & value) throw (HKLException)
                  {
                    if (Omega::isValid())
                      {
                        double const & alpha = m_geometry.get_alpha();
                        double komega;
                        double kappa = m_geometry.m_kappa.get_value();
                        double kphi = m_geometry.m_kphi.get_value();

                        double const & omega = value;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                        double p = asin(tan(chi/2.)/tan(alpha));
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                        kphi = phi + p + constant::math::pi/2.;

                        m_geometry.m_komega.set_value(komega);
                        m_geometry.m_kappa.set_value(kappa);
                        m_geometry.m_kphi.set_value(kphi);
                      }
                  }

                /*****************/
                /* CHI PSEUDOAXE */
                /*****************/
                Chi::Chi(geometry::kappa4C::Vertical & geometry) :
                  PseudoAxe<geometry::kappa4C::Vertical>(geometry)
                {
                  set_name ("chi");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                  m_initialized = true;
                  m_writable = true;
                }

                Chi::~Chi(void)
                  {}

                double
                Chi::get_min(void) const
                  {
                    double min = 0;
                    if (m_initialized)
                        min = -m_geometry.get_alpha() * 2;
                    return min;
                  }

                double
                Chi::get_max(void) const
                  {
                    double max = 0;
                    if (m_initialized)
                        max = m_geometry.get_alpha() * 2;
                    return max;
                  }

                bool
                Chi::isValid(void) throw (HKLException)
                  {
                    if (PseudoAxe<geometry::kappa4C::Vertical>::isValid())
                      {
                        try
                          {
                            m_geometry.isValid();
                          }
                        catch (HKLException &)
                          {
                            m_writable = false;
                            throw;
                          }
                      }
                    return true;
                  }

                double
                Chi::get_value(void) throw (HKLException)
                  {
                    if (Chi::isValid())
                      {
                        double const & alpha = m_geometry.get_alpha();
                        double const & kappa = m_geometry.m_kappa.get_value();
                        return -2 * asin(sin(kappa/2.) * sin(alpha));
                      }
                  }

                void
                Chi::set_value(double const & value) throw (HKLException)
                  {
                    if (Chi::isValid())
                      {
                        double const & alpha = m_geometry.get_alpha();
                        if (fabs(value) <= 2 * alpha)
                          {

                            double komega = m_geometry.m_komega.get_value();
                            double kappa = m_geometry.m_kappa.get_value();
                            double kphi = m_geometry.m_kphi.get_value();

                            double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                            double const & chi = value;
                            double phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                            double p = asin(tan(chi/2.)/tan(alpha));
                            komega = omega + p - constant::math::pi/2.;
                            kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                            kphi = phi + p + constant::math::pi/2.;

                            m_geometry.m_komega.set_value(komega);
                            m_geometry.m_kappa.set_value(kappa);
                            m_geometry.m_kphi.set_value(kphi);
                          }
                        else
                          {
                            ostringstream reason;
                            reason << "Unreachable \"" << get_name() << "\" PseudoAxe value"; 
                            HKLEXCEPTION(reason.str(),
                                         "|chi| <= 2 * alpha");
                          }
                      }
                  }

                /*****************/
                /* PHI PSEUDOAXE */
                /*****************/
                Phi::Phi(geometry::kappa4C::Vertical & geometry) :
                  PseudoAxe<geometry::kappa4C::Vertical>(geometry)
                {
                  set_name ("phi");
                  set_description ("This is the value of an equivalent eulerian geometry.");
                  m_initialized = true;
                  m_writable = true;
                }

                Phi::~Phi(void)
                  {}

                double
                Phi::get_min(void) const
                  {
                    double min = 0;
                    if (m_initialized)
                        min = -constant::math::pi;
                    return min;
                  }

                double
                Phi::get_max(void) const
                  {
                  double max = 0;
                    if (m_initialized)
                        max = constant::math::pi;
                    return max;
                  }

                bool
                Phi::isValid(void) throw (HKLException)
                  {
                    if(PseudoAxe<geometry::kappa4C::Vertical>::isValid())
                      {
                        try
                          {
                            m_geometry.isValid();
                          }
                        catch (HKLException &)
                          {
                            m_writable = false;
                            throw;
                          }
                      }
                    return true;
                  }

                double
                Phi::get_value(void) throw (HKLException)
                  {
                    if (Phi::isValid())
                      {
                        double const & alpha = m_geometry.get_alpha();
                        double const & kappa = m_geometry.m_kappa.get_value();
                        double const & kphi = m_geometry.m_kphi.get_value();

                        return kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;
                      }
                  }

                void
                Phi::set_value(double const & value) throw (HKLException)
                  {
                    if (Phi::isValid())
                      {
                        double const & alpha = m_geometry.get_alpha();
                        double komega = m_geometry.m_komega.get_value();
                        double kappa = m_geometry.m_kappa.get_value();
                        double kphi;

                        double omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;
                        double chi = -2 * asin(sin(kappa/2.) * sin(alpha));
                        double const & phi = value;

                        double p = asin(tan(chi/2.)/tan(alpha));
                        komega = omega + p - constant::math::pi/2.;
                        kappa = -2 * asin(sin(chi/2.)/sin(alpha));
                        kphi = phi + p + constant::math::pi/2.;

                        m_geometry.m_komega.set_value(komega);
                        m_geometry.m_kappa.set_value(kappa);
                        m_geometry.m_kphi.set_value(kphi);
                      }
                  }

            } // namespace vertical
        } // namespace kappa4C
    } // namespace pseudoAxe
} // namespace hkl
