#include "pseudoaxe_eulerian6C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian6C {

            Eulerian6C::Eulerian6C(void)
            : PseudoAxe()
              {}

            Eulerian6C::~Eulerian6C(void)
              {}

            ostream &
            Eulerian6C::toStream(ostream & flux) const
              {
                PseudoAxe::toStream(flux);
                m_geometry.toStream (flux);

                return flux;
              }

            istream &
            Eulerian6C::fromStream(istream & flux)
              {
                PseudoAxe::fromStream(flux);
                m_geometry.fromStream (flux);

                return flux;
              }

            /*****************/
            /* TTH PSEUDOAXE */
            /*****************/
            Tth::Tth(void) :
              Eulerian6C()
            {
              set_name ("2theta");
              set_description ("2theta = 2 * theta.");
            }

            Tth::~Tth(void)
              {}

            void
            Tth::initialize(Geometry const & geometry) throw (HKLException)
              {
                m_geometry = dynamic_cast<geometry::Eulerian6C const &>(geometry);
                Tth::set_wasInitialized(true);
              }

            bool
            Tth::get_isValid(Geometry const & geometry) const
              {
                if (Tth::get_wasInitialized())
                  {
                    return true;
                    svector ki0 = m_geometry.get_source().getKi();
                    svector kf0 = m_geometry.getKf();
                    svector axe0 = ki0.vectorialProduct(kf0);

                    svector ki = geometry.get_source().getKi();
                    svector kf = geometry.getKf();
                    svector axe = ki.vectorialProduct(kf);

                    svector test = axe.vectorialProduct(axe0);
                    if (test == svector())
                        return true;
                  }
                return false;
              }

            double
            Tth::get_value(Geometry const & geometry) const throw (HKLException)
              {
                double gamma = geometry.get_axe("gamma").get_value();
                double delta = geometry.get_axe("delta").get_value();
                double value = acos(cos(gamma)*cos(delta));

                if (Tth::get_wasInitialized())
                  {
                    svector ki0 = m_geometry.get_source().getKi();
                    svector kf0 = m_geometry.getKf();
                    svector axe0 = (ki0.vectorialProduct(kf0)).normalize();
                    svector ki = geometry.get_source().getKi();
                    svector kf = geometry.getKf();
                    svector axe = (ki.vectorialProduct(kf));
                    if (axe.norm2() < constant::math::epsilon_0) // we are close to pi or -pi
                        return constant::math::pi;
                    else
                      {
                        axe = axe.normalize();
                        if ((fabs(axe[X] - axe0[X]) < constant::math::epsilon_1)
                            && (fabs(axe[Y] - axe0[Y]) < constant::math::epsilon_1)
                            && (fabs(axe[Z] - axe0[Z]) < constant::math::epsilon_1))
                          {
                            return value;
                          }
                        else if ((fabs(axe[X] + axe0[X]) < constant::math::epsilon_1)
                                 && (fabs(axe[Y] + axe0[Y]) < constant::math::epsilon_1)
                                 && (fabs(axe[Z] + axe0[Z]) < constant::math::epsilon_1))
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
            Tth::set_value(Geometry & geometry,
                           double const & value) const throw (HKLException)
              {
                if (Tth::get_wasInitialized())
                  {
                    svector ki0 = m_geometry.get_source().getKi();
                    svector kf0 = m_geometry.getKf();
                    svector axe0 = ki0.vectorialProduct(kf0);
                    svector ki = geometry.get_source().getKi();
                    svector kf = ki.rotatedAroundVector(axe0, value);

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
#ifdef MSVC6
              PseudoAxe()
#else
              pseudoAxe::eulerian6C::Tth()
#endif
            {
              set_name("q");
              set_description ("q = 2 * tau * sin(theta) / lambda");
#ifdef MSVC6
              set_valueList(m_tth.get_valueList());
#endif
            }

            Q::~Q(void)
              {
              }

            void
            Q::initialize(Geometry const & geometry) throw (HKLException)
              {
                double lambda = geometry.get_source().get_waveLength();
                if (fabs(lambda) > constant::math::epsilon_0)
                  {
#ifdef MSVC6
                    m_tth.set_valueList(get_valueList());
                    m_tth.initialize(geometry);
#else
                    pseudoAxe::eulerian6C::Tth::initialize(geometry);
#endif
                    //Q::set_wasInitialized(true);
                  }
                else
                    HKLEXCEPTION("The source is not properly set.",
                                 "Please set the source wave length.");
              }

            bool
            Q::get_isValid(Geometry const & geometry) const
              {
                double lambda = geometry.get_source().get_waveLength();
                if (fabs(lambda) > constant::math::epsilon_0)
                  {
#ifdef MSVC6
                    m_tth.set_valueList(get_valueList());
                    return m_tth.get_isValid(geometry);
#else
                    return pseudoAxe::eulerian6C::Tth::get_isValid(geometry);
#endif
                  }
                else
                    return false;
              }

            double
            Q::get_value(Geometry const & geometry) const throw (HKLException)
              {
                double lambda = geometry.get_source().get_waveLength();
                if (fabs(lambda) > constant::math::epsilon_0)
                  {
                    double theta;
#ifdef MSVC6
                    m_tth.set_valueList(get_valueList());
                    theta = m_tth.get_value(geometry) / 2.;
#else
                    theta = pseudoAxe::eulerian6C::Tth::get_value(geometry) / 2.;
#endif
                    double value = 2 * constant::physic::tau * sin(theta) / geometry.get_source().get_waveLength();
                    return value;
                  }
                else
                    HKLEXCEPTION("The source is not properly set.",
                                 "Please set the source wave length.");

              }

            void
            Q::set_value(Geometry & geometry,
                         double const & value) const throw (HKLException)
              {
                if (Q::get_isValid(geometry))
                  {
                    double lambda = geometry.get_source().get_waveLength();
                    double two_theta = 2 * asin(value * lambda / (2 * constant::physic::tau));
#ifdef MSVC6
                    m_tth.set_valueList(get_valueList());
                    m_tth.set_value(geometry, two_theta);
#else
                    pseudoAxe::eulerian6C::Tth::set_value(geometry, two_theta);
#endif
                  }
                else
                    HKLEXCEPTION("The source is not properly set.",
                                 "Please set the source wave length.");
              }

            namespace eulerian4C {
                namespace vertical {

                    /*****************/
                    /* PSI PSEUDOAXE */
                    /*****************/
                    Psi::Psi(void) :
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::eulerian4C::vertical::Psi()
#endif
                    {
                      set_name("psi_v");
#ifdef MSVC6
                      set_description(m_psi.get_description());
                      set_valueList(m_psi.get_valueList());
#endif
                    }

                    Psi::~Psi(void)
                      {
                      }

                    void
                    Psi::initialize(Geometry const & geometry) throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi.set_valueList(get_valueList());
                        m_psi.initialize(m_E4CV);
#else
                        pseudoAxe::eulerian4C::vertical::Psi::initialize(m_E4CV);
#endif
                      }

                    bool
                    Psi::get_isValid(Geometry const & geometry) const
                      {
                        m_E4CV.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi.set_valueList(get_valueList());
                        return m_psi.get_isValid(m_E4CV);
#else
                        return pseudoAxe::eulerian4C::vertical::Psi::get_isValid(m_E4CV);
#endif
                      }

                    double
                    Psi::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi.set_valueList(get_valueList());
                        return m_psi.get_value(m_E4CV);
#else
                        return pseudoAxe::eulerian4C::vertical::Psi::get_value(m_E4CV);
#endif
                      }

                    void
                    Psi::set_value(Geometry & geometry,
                                   double const & value) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi.set_valueList(get_valueList());
                        m_psi.set_value(m_E4CV, value);
#else
                        pseudoAxe::eulerian4C::vertical::Psi::set_value(m_E4CV, value);
#endif
                        geometry.setFromGeometry(m_E4CV, false);
                      }

                } // namespace vertical
            } // namespace eulerian4C
        } // namespace eulerian6C
    } // namespace pseudoAxe
} // namespace hkl
