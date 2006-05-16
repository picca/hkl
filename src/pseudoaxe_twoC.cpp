#include <sstream>

#include "pseudoaxe_twoC.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace twoC {

            Vertical::Vertical(void)
            : PseudoAxe()
              {}

            Vertical::~Vertical(void)
              {}

            ostream &
            Vertical::toStream(ostream & flux) const
              {
                PseudoAxe::toStream(flux);
                m_geometry.toStream (flux);

                return flux;
              }

            istream &
            Vertical::fromStream(istream & flux)
              {
                PseudoAxe::fromStream(flux);
                m_geometry.fromStream (flux);

                return flux;
              }

            namespace vertical {
                /*******************/
                /* TH2TH PSEUDOAXE */
                /*******************/
                Th2th::Th2th(void) :
                  Vertical()
                {
                  set_name ("th2th");
                  set_description ("domega = 1/2 * d2theta.");
                }

                Th2th::~Th2th(void)
                  {}

                void
                Th2th::initialize(Geometry const & geometry)
                  {
                    m_geometry = dynamic_cast<geometry::twoC::Vertical const &>(geometry);
                    set_wasInitialized(true);
                  }

                bool
                Th2th::get_isValid(Geometry const & geometry) const
                  {
                    double omega0 = m_geometry.get_axe("omega").get_value();
                    double two_theta0 = m_geometry.get_axe("2theta").get_value();
                    double omega = geometry.get_axe("omega").get_value();
                    double two_theta = geometry.get_axe("2theta").get_value();
                   
                    if (fabs(omega - omega0 - (two_theta - two_theta0) / 2) < constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Th2th::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    if (get_isValid(geometry))
                        return geometry.get_axe("2theta").get_value();
                    else
                      {
                        ostringstream reason;
                        ostringstream description;
                        reason << "the current geometry is not compatible with the \"" << get_name() << "\" initialization";
                        description << "please initilize the pseudoAxe \"" << get_name() << "\".";
                        throw HKLException(reason.str(),
                                           description.str(),
                                           "hkl::pseudoAxe::twoC::vertical::Th2th::get_value(Geometry const &)");
                      }
                  }

                void
                Th2th::set_value(Geometry & geometry,
                                 double const & value) const throw (HKLException)
                  {
                    if (get_wasInitialized())
                      {
                        Axe & Omega = geometry.get_axe("omega");
                        Axe & Two_theta = geometry.get_axe("2theta");
                        double omega0 = m_geometry.get_axe("omega").get_value();
                        double two_theta0 = m_geometry.get_axe("2theta").get_value();

                        double two_theta = value;
                        double omega = omega0 + (two_theta - two_theta0) / 2.;

                        Omega.set_value(omega);
                        Two_theta.set_value(two_theta);
                      }
                    else
                      {
                        ostringstream reason;
                        ostringstream description;
                        reason << "pseudoAxe \"" << get_name() << "\" was not initialized.";
                        description << "please initilize the pseudoAxe \"" << get_name() << "\".";
                        throw HKLException(reason.str(),
                                           description.str(),
                                           "pseudoAxe::twoC::Q2th::set_value(Geometry &, double const &");
                      }
                  }

                /******************/
                /* Q2TH PSEUDOAXE */
                /******************/
                Q2th::Q2th(void) :
                  Vertical()
                {
                  set_name ("q2th");
                  set_description ("domega = 1/2 * d2theta.");
                }

                Q2th::~Q2th(void)
                  {}

                void
                Q2th::initialize(Geometry const & geometry)
                  {
                    m_geometry = dynamic_cast<geometry::twoC::Vertical const &>(geometry);
                    set_wasInitialized(true);
                  }

                bool
                Q2th::get_isValid(Geometry const & geometry) const
                  {
                    double omega0 = m_geometry.get_axe("omega").get_value();
                    double two_theta0 = m_geometry.get_axe("2theta").get_value();
                    double omega = geometry.get_axe("omega").get_value();
                    double two_theta = geometry.get_axe("2theta").get_value();

                    if (fabs(omega - omega0 - (two_theta - two_theta0) / 2) < constant::math::epsilon_0)
                        return true;
                    else
                        return false;
                  }

                double
                Q2th::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    if (get_isValid(geometry))
                      {
                        double theta = geometry.get_axe("2theta").get_value() / 2.;
                        double value = 2 * constant::physic::tau * sin(theta) / geometry.get_source().get_waveLength();
                        return value;
                      }
                    else
                      {
                        ostringstream reason;
                        ostringstream description;
                        reason << "the current geometry is not compatible with the \"" << get_name() << "\" initialization";
                        description << "please initilize the pseudoAxe \"" << get_name() << "\".";
                        throw HKLException(reason.str(),
                                           description.str(),
                                           "hkl::pseudoAxe::twoC::vertical::Q2th::get_value(Geometry const &)");
                      }
                  }

                void
                Q2th::set_value(Geometry & geometry,
                                double const & value) const throw (HKLException)
                  {
                    if (get_wasInitialized())
                      {
                        Axe & Omega = geometry.get_axe("omega");
                        Axe & Two_theta = geometry.get_axe("2theta");
                        double omega0 = m_geometry.get_axe("omega").get_value();
                        double two_theta0 = m_geometry.get_axe("2theta").get_value();

                        double lambda = geometry.get_source().get_waveLength();
                        double two_theta = 2 * asin(value * lambda / (2 * constant::physic::tau));
                        double omega = omega0 + (two_theta - two_theta0) / 2.;

                        Omega.set_value(omega);
                        Two_theta.set_value(two_theta);
                      }
                    else
                      {
                        ostringstream reason;
                        ostringstream description;
                        reason << "pseudoAxe \"" << get_name() << "\" was not initialized.";
                        description << "please initilize the pseudoAxe \"" << get_name() << "\".";
                        throw HKLException(reason.str(),
                                           description.str(),
                                           "pseudoAxe::twoC::Q2th::set_value(Geometry &, double const &");
                      }
                  }

                /***************/
                /* Q PSEUDOAXE */
                /***************/
                Q::Q(void) :
                  Vertical()
                {
                  set_name ("q");
                  set_description ("q = 2 * tau * sin(theta) / lambda");
                }

                Q::~Q(void)
                  {}

                void
                Q::initialize(Geometry const & geometry)
                  {}

                bool
                Q::get_isValid(Geometry const & geometry) const
                  {
                    return true;
                  }

                double
                Q::get_value(Geometry const & geometry) const throw (HKLException)
                  {
                    double theta = geometry.get_axe("2theta").get_value() / 2.;
                    double value = 2 * constant::physic::tau * sin(theta) / geometry.get_source().get_waveLength();
                    return value;
                  }

                void
                Q::set_value(Geometry & geometry,
                             double const & value) const throw (HKLException)
                  {
                    Axe & Two_theta = geometry.get_axe("2theta");

                    double lambda = geometry.get_source().get_waveLength();
                    double two_theta = 2 * asin(value * lambda / (2 * constant::physic::tau));

                    Two_theta.set_value(two_theta);
                  }

            } // namespace vertical
        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl
