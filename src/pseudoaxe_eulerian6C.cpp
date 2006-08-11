#include "pseudoaxe_eulerian6C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian6C {

            /*****************/
            /* TTH PSEUDOAXE */
            /*****************/
            Tth::Tth(geometry::Eulerian6C & geometry) : PseudoAxe<geometry::Eulerian6C>(geometry)
            {
              set_name("2theta");
              set_description ("2theta = 2 * theta.\n");
              addParameter("direction", 1., "Prefered mode when gamma=0 and delta=0\n  Vertical=1(default).\n  Horizontal=0.");
            }

            Tth::Tth(Tth const & pseudoAxe) :
              PseudoAxe<geometry::Eulerian6C>(pseudoAxe),
              m_axe(pseudoAxe.m_axe)
            {}

            Tth::~Tth(void)
              {}

            ostream &
            Tth::toStream(ostream & flux) const
              {
                PseudoAxe<geometry::Eulerian6C>::toStream(flux);
                m_axe.toStream(flux);

                return flux;
              }

            istream &
            Tth::fromStream(istream & flux)
              {
                PseudoAxe<geometry::Eulerian6C>::fromStream(flux);
                m_axe.fromStream(flux);

                return flux;
              }

            void
            Tth::initialize(void) throw (HKLException)
              {
                if (m_geometry.get_source().get_waveLength() > constant::math::epsilon_0)
                  {
                    m_geometry0 = m_geometry;
                    svector ki0 = m_geometry0.get_source().getKi();
                    svector kf0 = m_geometry0.getKf();
                    m_axe = ki0.vectorialProduct(kf0);
                    if (m_axe == svector())
                      {
                        if (getParameterValue("direction") == 1.)
                            m_axe = svector(0, -1, 0);
                        else
                            m_axe = svector(0, 0, 1);
                      }
                    else
                        m_axe = m_axe.normalize();
                    m_wasInitialized = true;
                  }
                else
                  {
                    ostringstream reason;
                    reason << "Can not initialize the \"" << get_name() << "\" pseudoAxe with a null wave length.";
                    HKLEXCEPTION(reason.str(), "please set a non-null wave length.");
                  }
              }

            bool
            Tth::get_isValid(void) const
              {
                return true;
              }

            double
            Tth::get_value(void) const throw (HKLException)
              {
                double gamma = m_geometry.get_axe("gamma").get_value();
                double delta = m_geometry.get_axe("delta").get_value();
                double value = acos(cos(gamma)*cos(delta));

                if (m_wasInitialized)
                  {
                    svector ki = m_geometry.get_source().getKi();
                    svector kf = m_geometry.getKf();
                    svector axe = (ki.vectorialProduct(kf));
                    if (axe.norm2() < constant::math::epsilon_0) // we are close to 0, pi or -pi
                        return 0;
                    else
                      {
                        axe = axe.normalize();
                        if ((fabs(axe[X] - m_axe[X]) < constant::math::epsilon_1)
                            && (fabs(axe[Y] - m_axe[Y]) < constant::math::epsilon_1)
                            && (fabs(axe[Z] - m_axe[Z]) < constant::math::epsilon_1))
                          {
                            return value;
                          }
                        else if ((fabs(axe[X] + m_axe[X]) < constant::math::epsilon_1)
                                 && (fabs(axe[Y] + m_axe[Y]) < constant::math::epsilon_1)
                                 && (fabs(axe[Z] + m_axe[Z]) < constant::math::epsilon_1))
                          {
                            return -value;
                          }
                        else
                          {
                            ostringstream reason;
                            reason << "Cannot get the value of the \"" << get_name() << "\" un-initialized pseudoAxe.";
                            HKLEXCEPTION(reason.str(), "please initialize it.");
                          }
                      }
                  }
                else
                    return value;
              }

            void
            Tth::set_value(double const & value) throw (HKLException)
              {
                if (m_wasInitialized)
                  {
                    svector ki = m_geometry.get_source().getKi();
                    svector kf = ki.rotatedAroundVector(m_axe, value);

                    // 1st solution
                    double gamma1 = atan2(kf[Y], kf[X]);
                    double delta1 = atan2(kf[Z], sqrt(kf[X]*kf[X]+kf[Y]*kf[Y]));
                    geometry::Eulerian6C g1(0, 0, 0, 0, gamma1, delta1);

                    // 2nd solution
                    double gamma2 = atan2(-kf[Y], -kf[X]);
                    double delta2 = atan2(kf[Z], -sqrt(kf[X]*kf[X]+kf[Y]*kf[Y]));
                    geometry::Eulerian6C g2(0, 0, 0, 0, gamma2, delta2);
                    if (m_geometry.getDistance(g1) < m_geometry.getDistance(g2))
                      {
                        m_geometry.get_axe("gamma").set_value(gamma1);
                        m_geometry.get_axe("delta").set_value(delta1);
                      }
                    else
                      {
                        m_geometry.get_axe("gamma").set_value(gamma2);
                        m_geometry.get_axe("delta").set_value(delta2);
                      }
                  }
                else
                  {
                    ostringstream reason;
                    reason << "Cannot set the value of the \"" << get_name() << "\" un-initialized pseudoaxe.";
                    HKLEXCEPTION(reason.str(), "please initialize it.");
                  }
              }

            /***************/
            /* Q PSEUDOAXE */
            /***************/
            Q::Q(geometry::Eulerian6C & geometry) :
              PseudoAxe<geometry::Eulerian6C>(geometry)
            {
              m_tth = new pseudoAxe::eulerian6C::Tth(geometry);
              set_name("q");
              set_description ("q = 2 * tau * sin(theta) / lambda");
              set_valueList(m_tth->get_valueList());
            }

            Q::~Q(void)
              {
                delete m_tth;
              }

            void
            Q::initialize(void) throw (HKLException)
              {
                m_tth->set_valueList(get_valueList());
                m_tth->initialize();
              }

            bool
            Q::get_isValid(void) const
              {
                double lambda = m_geometry.get_source().get_waveLength();
                if (lambda > constant::math::epsilon_0)
                    return true;
                else
                    return false;
              }

            double
            Q::get_value(void) const throw (HKLException)
              {
                double lambda = m_geometry.get_source().get_waveLength();
                if (lambda > constant::math::epsilon_0)
                  {
                    double theta;
                    m_tth->set_valueList(get_valueList());
                    theta = m_tth->get_value() / 2.;
                    double value = 2 * constant::physic::tau * sin(theta) / m_geometry.get_source().get_waveLength();
                    return value;
                  }
                else
                  {
                    ostringstream reason;
                    reason << "Cannot get the value of the \"" << get_name() << "\" pseudoAxe when the wave length is null.";
                    HKLEXCEPTION(reason.str(), "Please set a non-null wave length.");

                  }
              }

            void
            Q::set_value(double const & value) throw (HKLException)
              {
                if (Q::get_isValid())
                  {
                    double lambda = m_geometry.get_source().get_waveLength();
                    double two_theta = 2 * asin(value * lambda / (2 * constant::physic::tau));
                    m_tth->set_valueList(get_valueList());
                    m_tth->set_value(two_theta);
                  }
                else
                  {
                    ostringstream reason;
                    reason << "Cannot set the value of the \"" << get_name() << "\" pseudoAxe when the wave length is null.";
                    HKLEXCEPTION(reason.str(), "Please set a non-null wave length.");

                  }
              }

        } // namespace eulerian6C
    } // namespace pseudoAxe
} // namespace hkl
