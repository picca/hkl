#include <sstream>

#include "pseudoaxe_twoC.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace twoC {
            namespace vertical {

                /*******************/
                /* TH2TH PSEUDOAXE */
                /*******************/
                Th2th::Th2th(geometry::twoC::Vertical & geometry) :
                  PseudoAxe<geometry::twoC::Vertical>(geometry)
                {
                  set_name ("th2th");
                  set_description ("domega = 1/2 * d2theta.");
                }

                Th2th::~Th2th(void)
                  {}

                bool
                Th2th::isValid(void) throw (HKLException)
                  {
                    bool valid = false;
                    m_writable = false;
                    if (m_geometry.isValid() && m_initialized)
                      {
                        double omega0 = m_geometry0.m_omega.get_value();
                        double two_theta0 = m_geometry0.m_tth.get_value();
                        double omega = m_geometry.m_omega.get_value();
                        double two_theta = m_geometry.m_tth.get_value();

                        if (fabs(omega - omega0 - (two_theta - two_theta0) / 2) < constant::math::epsilon_0)
                          {
                            m_writable = true;
                            valid = true;
                          }
                      }
                    return valid;
                  }

                double
                Th2th::get_min(void) const
                  {
                    double min = m_geometry.m_tth.get_min();
                    try
                      {
                        m_geometry.isValid();
                      }
                    catch (HKLException &)
                      {
                        min = 0;
                      }
                    return min;
                  }

                double
                Th2th::get_max(void) const
                  {
                    double max = m_geometry.m_tth.get_max();
                    try
                      {
                        m_geometry.isValid();
                      }
                    catch (HKLException &)
                      {
                        max = 0;
                      }
                    return max;
                  }

                double
                Th2th::get_value(void) throw (HKLException)
                  {
                    Th2th::isValid();
                    // the next line will not be executed if the m_geometry was not valid.
                    return m_geometry.m_tth.get_value();
                  }

                void
                Th2th::set_value(double const & value) throw (HKLException)
                  {
                    if (Th2th::isValid())
                      {
                        double omega0 = m_geometry0.m_omega.get_value();
                        double tth0 = m_geometry0.m_tth.get_value();

                        double tth = value;
                        double omega = omega0 + (tth - tth0) / 2.;

                        m_geometry.m_omega.set_value(omega);
                        m_geometry.m_tth.set_value(tth);
                      }
                      else
                          HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
                  }

                /******************/
                /* Q2TH PSEUDOAXE */
                /******************/
                Q2th::Q2th(geometry::twoC::Vertical & geometry) :
                  PseudoAxe<geometry::twoC::Vertical>(geometry)
                {
                  set_name ("q2th");
                  set_description ("domega = 1/2 * d2theta.");
                }

                Q2th::~Q2th(void)
                  {}

                bool
                Q2th::isValid(void) throw (HKLException)
                  {
                    bool valid = false;
                    m_writable = false;
                    if (m_geometry.isValid() && m_initialized)
                      {
                        double omega0 = m_geometry0.m_omega.get_value();
                        double two_theta0 = m_geometry0.m_tth.get_value();
                        double omega = m_geometry.m_omega.get_value();
                        double two_theta = m_geometry.m_tth.get_value();

                        if (fabs(omega - omega0 - (two_theta - two_theta0) / 2) < constant::math::epsilon_0)
                          {
                            m_writable = true;
                            valid = true;
                          }
                      }
                    return valid;
                  }

                double
                Q2th::get_min(void) const
                  {
                    double min;
                    try
                      {
                        m_geometry.isValid();
                        double lambda = m_geometry.get_source().get_waveLength();
                        min = -2 * constant::physic::tau / lambda;
                      }
                    catch (HKLException &)
                      {
                        min = 0;
                      }
                    return min;
                  }

                double
                Q2th::get_max(void) const
                  {
                    double max;
                    try
                      {
                        m_geometry.isValid();
                        double lambda = m_geometry.get_source().get_waveLength();
                        max = 2 * constant::physic::tau / lambda;
                      }
                    catch (HKLException &)
                      {
                        max = 0;
                      }
                    return max;
                  }

                double
                Q2th::get_value(void) throw (HKLException)
                  {
                    Q2th::isValid();
                    // next lines will not be executed if the source is not well set.
                    double lambda = m_geometry.get_source().get_waveLength();
                    double theta = m_geometry.m_tth.get_value() / 2.;
                    return 2 * constant::physic::tau * sin(theta) / lambda;
                  }

                void
                Q2th::set_value(double const & value) throw (HKLException)
                  {
                    if (Q2th::isValid())
                      {
                        double lambda = m_geometry.get_source().get_waveLength();
                        double omega0 = m_geometry0.m_omega.get_value();
                        double tth0 = m_geometry0.m_tth.get_value();

                        double tth = 2 * asin(value * lambda / (2 * constant::physic::tau));
                        double omega = omega0 + (tth - tth0) / 2.;

                        m_geometry.m_omega.set_value(omega);
                        m_geometry.m_tth.set_value(tth);
                      }
                    else
                        HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
                  }

                /***************/
                /* Q PSEUDOAXE */
                /***************/
                Q::Q(geometry::twoC::Vertical & geometry) :
                  PseudoAxe<geometry::twoC::Vertical>(geometry)
                {
                  set_name ("q");
                  set_description ("q = 2 * tau * sin(theta) / lambda");
                }

                Q::~Q(void)
                  {}

                double
                Q::get_min(void) const
                  {
                    double min;
                    try
                      {
                        m_geometry.isValid(); // throw
                        double lambda = m_geometry.get_source().get_waveLength();
                        min = -2 * constant::physic::tau / lambda;
                      }
                    catch (HKLException &)
                      {
                        min = 0;
                      }
                    return min;
                  }

                double
                Q::get_max(void) const
                  {
                    double max;
                    try
                      {
                        m_geometry.isValid(); // throw
                        double lambda = m_geometry.get_source().get_waveLength();
                        max = 2 * constant::physic::tau / lambda;
                      }
                    catch (HKLException &)
                      {
                        max = 0;
                      }
                    return max;
                  }

                bool
                Q::isValid(void) throw (HKLException)
                  {
                    bool valid = false;
                    m_writable = false;
                    if (m_geometry.isValid() && m_initialized)
                      {
                        m_writable = true;
                        valid = true;
                      }
                    return valid;
                  }

                double
                Q::get_value(void) throw (HKLException)
                  {
                    Q::isValid();
                    // next lines will not be executed if the source is not well set.
                    double lambda = m_geometry.get_source().get_waveLength();
                    double theta = m_geometry.m_tth.get_value() / 2.;
                    return 2 * constant::physic::tau * sin(theta) / lambda;
                  }

                void
                Q::set_value(double const & value) throw (HKLException)
                  {
                    if (Q::isValid())
                      {
                        double lambda = m_geometry.get_source().get_waveLength();
                        double tth = 2 * asin(value * lambda / (2 * constant::physic::tau));
                        m_geometry.m_tth.set_value(tth);
                      }
                    else
                        HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
                  }

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
