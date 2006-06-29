#include "pseudoaxe_eulerian6C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian6C {

            /*****************/
            /* TTH PSEUDOAXE */
            /*****************/
            Tth::Tth(void) : PseudoAxe<geometry::Eulerian6C>()
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
            Tth::initialize(geometry::Eulerian6C const & geometry) throw (HKLException)
              {
                if (geometry.get_source().get_waveLength() > constant::math::epsilon_0)
                  {
                    m_geometry = geometry;
                    svector ki0 = m_geometry.get_source().getKi();
                    svector kf0 = m_geometry.getKf();
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
                    reason << "Can not initialize the \"" << get_name() << "\" pseudoAxe.";
                    HKLEXCEPTION(reason.str(), "please set properly the wavelength");
                  }
              }

            bool
            Tth::get_isValid(geometry::Eulerian6C const & geometry) const
              {
                return true;
              }

            double
            Tth::get_value(geometry::Eulerian6C const & geometry) const throw (HKLException)
              {
                double gamma = geometry.get_axe("gamma").get_value();
                double delta = geometry.get_axe("delta").get_value();
                double value = acos(cos(gamma)*cos(delta));

                if (m_wasInitialized)
                  {
                    svector ki = geometry.get_source().getKi();
                    svector kf = geometry.getKf();
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
                            ostringstream description;
                            reason << "The current Geometry is not compatible with the \"" << get_name() << "\" pseudoAxe initialization.";
                            description << "please initialize the pseudoAxe \"" << get_name() << "\".";
                            HKLEXCEPTION(reason.str(), description.str());
                          }
                      }
                  }
                else
                    return value;
              }

            void
            Tth::set_value(geometry::Eulerian6C & geometry,
                           double const & value) const throw (HKLException)
              {
                if (m_wasInitialized)
                  {
                    svector ki = geometry.get_source().getKi();
                    svector kf = ki.rotatedAroundVector(m_axe, value);

                    // 1st solution
                    double gamma1 = atan2(kf[Y], kf[X]);
                    double delta1 = atan2(kf[Z], sqrt(kf[X]*kf[X]+kf[Y]*kf[Y]));
                    geometry::Eulerian6C g1(0, 0, 0, 0, gamma1, delta1);

                    // 2nd solution
                    double gamma2 = atan2(-kf[Y], -kf[X]);
                    double delta2 = atan2(kf[Z], -sqrt(kf[X]*kf[X]+kf[Y]*kf[Y]));
                    geometry::Eulerian6C g2(0, 0, 0, 0, gamma2, delta2);
                    if (geometry.getDistance(g1) < geometry.getDistance(g2))
                      {
                        geometry.get_axe("gamma").set_value(gamma1);
                        geometry.get_axe("delta").set_value(delta1);
                      }
                    else
                      {
                        geometry.get_axe("gamma").set_value(gamma2);
                        geometry.get_axe("delta").set_value(delta2);
                      }
                  }
                else
                  {
                    ostringstream reason;
                    ostringstream description;
                    reason << "pseudoAxe \"" << get_name() << "\" was not initialized.";
                    description << "please initilize the pseudoAxe \"" << get_name() << "\".";
                    HKLEXCEPTION(reason.str(), description.str());
                  }
              }

            /***************/
            /* Q PSEUDOAXE */
            /***************/
            Q::Q(void) :
              PseudoAxe<geometry::Eulerian6C>()
            {
              set_name("q");
              set_description ("q = 2 * tau * sin(theta) / lambda");
              set_valueList(m_tth.get_valueList());
            }

            Q::~Q(void)
              {}

            void
            Q::initialize(geometry::Eulerian6C const & geometry) throw (HKLException)
              {
                m_tth.set_valueList(get_valueList());
                m_tth.initialize(geometry);
              }

            bool
            Q::get_isValid(geometry::Eulerian6C const & geometry) const
              {
                double lambda = geometry.get_source().get_waveLength();
                if (lambda > constant::math::epsilon_0)
                    return true;
                else
                    return false;
              }

            double
            Q::get_value(geometry::Eulerian6C const & geometry) const throw (HKLException)
              {
                double lambda = geometry.get_source().get_waveLength();
                if (lambda > constant::math::epsilon_0)
                  {
                    double theta;
                    m_tth.set_valueList(get_valueList());
                    theta = m_tth.get_value(geometry) / 2.;
                    double value = 2 * constant::physic::tau * sin(theta) / geometry.get_source().get_waveLength();
                    return value;
                  }
                else
                    HKLEXCEPTION("The source is not properly set.",
                                 "Please set the source wave length.");

              }

            void
            Q::set_value(geometry::Eulerian6C & geometry,
                         double const & value) const throw (HKLException)
              {
                if (Q::get_isValid(geometry))
                  {
                    double lambda = geometry.get_source().get_waveLength();
                    double two_theta = 2 * asin(value * lambda / (2 * constant::physic::tau));
                    m_tth.set_valueList(get_valueList());
                    m_tth.set_value(geometry, two_theta);
                  }
                else
                    HKLEXCEPTION("The source is not properly set.",
                                 "Please set the source wave length.");
              }

        } // namespace eulerian6C
    } // namespace pseudoAxe
} // namespace hkl
