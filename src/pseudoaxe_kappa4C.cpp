#include "pseudoaxe_kappa4C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {

        Kappa4C::Kappa4C(double alpha) :
          PseudoAxe()
        {
          m_geometry_K4C = new geometry::Kappa4C(alpha);
        }

        Kappa4C::~Kappa4C(void)
          {
            delete m_geometry_K4C;
          }

        ostream &
        Kappa4C::toStream(ostream & flux) const
          {
            PseudoAxe::toStream (flux);
            m_geometry_K4C->toStream (flux);
            flux << " " << m_omega;
            flux << " " << m_chi;
            flux << " " << m_phi << endl;

            return flux;
          }

        istream &
        Kappa4C::fromStream(istream & flux)
          {
            PseudoAxe::fromStream (flux);
            m_geometry_K4C->fromStream (flux);
            flux >> m_omega >> m_chi >> m_phi; 

            return flux;
          }

        namespace kappa4C {
            /*******************/
            /* OMEGA PSEUDOAXE */
            /*******************/
            Omega::Omega(double alpha) :
              Kappa4C (alpha)
            {
              set_name ("omega");
              set_description ("This is the value of an equivalent eulerian geometry.");
            }

            Omega::~Omega(void)
              {}

            void
            Omega::initialize(Geometry const & geometry)
              {
                set_wasInitialized(true);
              }

            bool
            Omega::get_isValid(Geometry const & geometry) const
              {
                return true;
              }

            double const 
            Omega::get_value(Geometry const & geometry)
              {
                double const & komega = geometry.get_axe("komega").get_value();
                double const & kappa = geometry.get_axe("kappa").get_value();
                double const & alpha = m_geometry_K4C->get_alpha();

                m_omega = komega + atan(tan(kappa/2.) * cos(alpha)) + constant::math::pi/2.;

                //cout << " obtained : " << value * constant::math::radToDeg << " " << psi_axe << " " << m_Q << endl;

                return m_omega;
              }

            void
            Omega::set_value(Geometry & geometry,
                             double value) throw (HKLException)
              {
                m_omega = value;
                double const & alpha = m_geometry_K4C->get_alpha();
                double p = asin(tan(m_chi/2.)/tan(alpha));
                double komega = m_omega + p - constant::math::pi/2.;
                double kappa = -2 * asin(sin(m_chi/2.)/sin(alpha));
                double kphi = m_phi + p + constant::math::pi/2.;
                
                geometry.get_axe("komega").set_value(komega);
                geometry.get_axe("kappa").set_value(kappa);
                geometry.get_axe("kphi").set_value(kphi);
              }

            /*****************/
            /* CHI PSEUDOAXE */
            /*****************/
            Chi::Chi(double alpha) :
              Kappa4C (alpha)
            {
              set_name ("chi");
              set_description ("This is the value of an equivalent eulerian geometry.");
            }

            Chi::~Chi(void)
              {}

            void
            Chi::initialize(Geometry const & geometry)
              {
                set_wasInitialized(true);
              }

            bool
            Chi::get_isValid(Geometry const & geometry) const
              {
                return true;
              }

            double const 
            Chi::get_value(Geometry const & geometry)
              {
                double const & kappa = geometry.get_axe("kappa").get_value();
                double const & alpha = m_geometry_K4C->get_alpha();

                m_chi = -2 * asin(sin(kappa/2.) * sin(alpha));

                //cout << " obtained : " << value * constant::math::radToDeg << " " << psi_axe << " " << m_Q << endl;

                return m_chi;
              }

            void
            Chi::set_value(Geometry & geometry,
                             double value) throw (HKLException)
              {
                m_chi = value;

                double const & alpha = m_geometry_K4C->get_alpha();
                double p = asin(tan(m_chi/2.)/tan(alpha));
                double komega = m_omega + p - constant::math::pi/2.;
                double kappa = -2 * asin(sin(m_chi/2.)/sin(alpha));
                double kphi = m_phi + p + constant::math::pi/2.;
                
                geometry.get_axe("komega").set_value(komega);
                geometry.get_axe("kappa").set_value(kappa);
                geometry.get_axe("kphi").set_value(kphi);
              }

            /*****************/
            /* PHI PSEUDOAXE */
            /*****************/
            Phi::Phi(double alpha) :
              Kappa4C (alpha)
            {
              set_name ("phi");
              set_description ("This is the value of an equivalent eulerian geometry.");
            }

            Phi::~Phi(void)
              {}

            void
            Phi::initialize(Geometry const & geometry)
              {
                set_wasInitialized(true);
              }

            bool
            Phi::get_isValid(Geometry const & geometry) const
              {
                return true;
              }

            double const 
            Phi::get_value(Geometry const & geometry)
              {
                double const & kappa = geometry.get_axe("kappa").get_value();
                double const & kphi = geometry.get_axe("kphi").get_value();
                double const & alpha = m_geometry_K4C->get_alpha();

                m_phi = kphi + atan(tan(kappa/2.) * cos(alpha)) - constant::math::pi/2.;

                //cout << " obtained : " << value * constant::math::radToDeg << " " << psi_axe << " " << m_Q << endl;

                return m_phi;
              }

            void
            Phi::set_value(Geometry & geometry,
                             double value) throw (HKLException)
              {
                m_phi = value;

                double const & alpha = m_geometry_K4C->get_alpha();
                double p = asin(tan(m_chi/2.)/tan(alpha));
                double komega = m_omega + p - constant::math::pi/2.;
                double kappa = -2 * asin(sin(m_chi/2.)/sin(alpha));
                double kphi = m_phi + p + constant::math::pi/2.;
                
                geometry.get_axe("komega").set_value(komega);
                geometry.get_axe("kappa").set_value(kappa);
                geometry.get_axe("kphi").set_value(kphi);
              }

        }			// namespace eulerian4C
    }				// namespace pseudoAxe
}				// namespace hkl
