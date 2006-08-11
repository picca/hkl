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

                void
                Th2th::initialize(void) throw (HKLException)
                  {
                    m_geometry0 = m_geometry;
                    m_wasInitialized = true;
                  }

                bool
                Th2th::get_isValid(void) const
                  {
                    if (m_wasInitialized)
                      {
                        double omega0 = m_geometry0.m_omega.get_value();
                        double two_theta0 = m_geometry0.m_tth.get_value();
                        double omega = m_geometry.m_omega.get_value();
                        double two_theta = m_geometry.m_tth.get_value();

                        if (fabs(omega - omega0 - (two_theta - two_theta0) / 2) < constant::math::epsilon_0)
                            return true;
                      }
                    return false;
                  }

                double
                Th2th::get_value(void) const throw (HKLException)
                  {
                    if (Th2th::get_isValid())
                        return m_geometry.m_tth.get_value();
                    else
                      {
                        ostringstream reason;
                        reason << "The \"" << get_name() << "\" PseudoAxe is not valid."; 
                        HKLEXCEPTION(reason.str(),
                                     "The current geometry is not compatible with the PseudoAxe initialization, please re-initialize it.");
                      }
                  }

                void
                Th2th::set_value(double const & value) throw (HKLException)
                  {
                    if (m_wasInitialized)
                      {
                        double omega0 = m_geometry0.m_omega.get_value();
                        double tth0 = m_geometry0.m_tth.get_value();

                        double tth = value;
                        double omega = omega0 + (tth - tth0) / 2.;

                        m_geometry.m_omega.set_value(omega);
                        m_geometry.m_tth.set_value(tth);
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "The \"" << get_name() << "\" PseudoAxe was not initialized.";
                        HKLEXCEPTION(reason.str(),
                                     "please initialize it.");
                      }
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

                void
                Q2th::initialize(void) throw (HKLException)
                  {
                    double lambda = m_geometry.get_source().get_waveLength();
                    if (fabs(lambda) > constant::math::epsilon_0)
                      {
                        m_geometry0 = m_geometry;
                        m_wasInitialized = true;
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "Cannot initialize the \"" << get_name() << "\" PseudoAxe when the wave length is null";
                        HKLEXCEPTION(reason.str(),
                                     "Please set a non-null wave length.");
                      }
                  }

                bool
                Q2th::get_isValid(void) const
                  {
                    if (m_wasInitialized && m_geometry.get_source().get_waveLength())
                      {
                        double omega0 = m_geometry0.m_omega.get_value();
                        double tth0 = m_geometry0.m_tth.get_value();
                        double omega = m_geometry.m_omega.get_value();
                        double tth = m_geometry.m_tth.get_value();

                        if (fabs(omega - omega0 - (tth - tth0) / 2) < constant::math::epsilon_0)
                            return true;
                      }
                    return false;
                  }

                double
                Q2th::get_value(void) const throw (HKLException)
                  {
                    if (Q2th::get_isValid())
                      {
                        double lambda = m_geometry.get_source().get_waveLength();
                        if (fabs(lambda) > constant::math::epsilon_0)
                          {
                            double theta = m_geometry.m_tth.get_value() / 2.;
                            double value = 2 * constant::physic::tau * sin(theta) / lambda;
                            return value;
                          }
                        else
                          {
                            ostringstream reason;
                            reason << "Cannot get the \"" << get_name() << "\" PseudoAxe value when the wave length is null.";
                            HKLEXCEPTION(reason.str(),
                                         "Please set a non-null wave length.");
                          }
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "The \"" << get_name() << "\" PseudoAxe is not valid."; 
                        HKLEXCEPTION(reason.str(),
                                     "The current geometry is not compatible with the PseudoAxe initialization, please re-initialize it.");
                      }
                  }

                void
                Q2th::set_value(double const & value) throw (HKLException)
                  {
                    if (m_wasInitialized)
                      {
                        double lambda = m_geometry.get_source().get_waveLength();
                        if (fabs(lambda) > constant::math::epsilon_0)
                          {
                            double omega0 = m_geometry0.m_omega.get_value();
                            double tth0 = m_geometry0.m_tth.get_value();

                            double tth = 2 * asin(value * lambda / (2 * constant::physic::tau));
                            double omega = omega0 + (tth - tth0) / 2.;

                            m_geometry.m_omega.set_value(omega);
                            m_geometry.m_tth.set_value(tth);
                          }
                        else
                          {
                            ostringstream reason;
                            reason << "Cannot set the \"" << get_name() << "\" PseudoAxe value when the wave length is null.";
                            HKLEXCEPTION(reason.str(),
                                         "Please set a non-null wave length.");
                          }
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "The \"" << get_name() << "\" PseudoAxe was not initialized.";
                        HKLEXCEPTION(reason.str(),
                                     "please initialize it.");
                      }
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

                void
                Q::initialize(void) throw (HKLException)
                  {
                    double lambda = m_geometry.get_source().get_waveLength();
                    if (fabs(lambda) > constant::math::epsilon_0)
                      {
                        m_wasInitialized = true;
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "Cannot initialize the \"" << get_name() << "\" PseudoAxe when the wave length is null";
                        HKLEXCEPTION(reason.str(),
                                     "Please set a non-null wave length.");
                      }
                  }

                bool
                Q::get_isValid(void) const
                  {
                    double lambda = m_geometry.get_source().get_waveLength();
                    if (fabs(lambda) > constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Q::get_value(void) const throw (HKLException)
                  {
                    if (Q::get_isValid())
                      {
                        double theta = m_geometry.m_tth.get_value() / 2.;
                        double value = 2 * constant::physic::tau * sin(theta) / m_geometry.get_source().get_waveLength();
                        return value;
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "Cannot get the \"" << get_name() << "\" PseudoAxe value when the wave length is null.";
                        HKLEXCEPTION(reason.str(),
                                     "Please set a non-null wave length.");
                      }
                  }

                void
                Q::set_value(double const & value) throw (HKLException)
                  {
                    if (Q::get_isValid())
                      {
                        double lambda = m_geometry.get_source().get_waveLength();
                        double tth = 2 * asin(value * lambda / (2 * constant::physic::tau));

                        m_geometry.m_tth.set_value(tth);
                      }
                    else
                      {
                        ostringstream reason;
                        reason << "Cannot set the \"" << get_name() << "\" PseudoAxe value when the wave length is null.";
                        HKLEXCEPTION(reason.str(),
                                     "Please set a non-null wave length.");
                      }
                  }

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
