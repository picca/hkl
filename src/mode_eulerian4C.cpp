#include "mode_eulerian4C.h"
#include "convenience.h"

namespace hkl
  {
  namespace mode
    {
    namespace eulerian4C
      {
      namespace vertical
        {

        /******************/
        /* BISSECTOR MODE */
        /******************/
        Bissector::Bissector(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry) :
            ModeTemp<geometry::eulerian4C::Vertical>(name, description, geometry)
        {}

        Bissector::~Bissector(void)
        {}

        void
        Bissector::computeAngles(Value const & h, Value const & k, Value const & l,
                                 smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              // Calcule de Omega
              double omega = theta;

              // Calcule de Chi
              double s_chi = hphi.y();
              double c_chi = hphi.x()*hphi.x()+hphi.z()*hphi.z();
              if (c_chi < 0.)
                HKLEXCEPTION("Unreachable reflection.",
                             "Change h k l values");
              else
                c_chi = sqrt(c_chi);
              double chi = convenience::atan2(s_chi, c_chi);

              // Calcule de Phi
              double s_phi = hphi.x();
              double c_phi = hphi.z();
              double phi = convenience::atan2(s_phi, c_phi);

              _geometry.omega()->set_current(omega);
              _geometry.chi()->set_current(chi);
              _geometry.phi()->set_current(phi);
              _geometry.tth()->set_current(2.*theta);
            }
        }

        /***************/
        /* DELTA THETA */
        /***************/

        Delta_Theta::Delta_Theta(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry) :
            ModeTemp<geometry::eulerian4C::Vertical>(name, description, geometry)
        {
          _dtheta = new Parameter("delta theta", "The omega offset relatively to theta.",
                                  0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_dtheta);
        }

        Delta_Theta::~Delta_Theta(void)
        {
          delete _dtheta;
        }

        void
        Delta_Theta::computeAngles(Value const & h, Value const & k, Value const & l,
                                   smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              // Calcule de Omega
              // By definition in 4C omega constant mode.
              double dtheta = _dtheta->get_current().get_value();
              double omega = theta + dtheta;

              // Calcule de Chi
              double s_chi = hphi.y();
              double c_chi = hphi.x()*hphi.x()-hphi.y()*hphi.y()*tan(dtheta)*tan(dtheta)+hphi.z()*hphi.z();
              if (c_chi < 0.)
                HKLEXCEPTION("Unreachable reflection.", "Change h k l values");
              else
                c_chi = sqrt(c_chi) * cos(dtheta);
              double chi = convenience::atan2(s_chi, c_chi);

              // Calcule de Phi
              double s_phi = hphi.x()*cos(dtheta)*cos(chi)-hphi.z()*sin(dtheta);
              double c_phi = hphi.z()*cos(dtheta)*cos(chi)+hphi.x()*sin(dtheta);
              double phi = convenience::atan2(s_phi, c_phi);

              _geometry.omega()->set_current(omega);
              _geometry.chi()->set_current(chi);
              _geometry.phi()->set_current(phi);
              _geometry.tth()->set_current(2.*theta);
            }
        }

        /******************/
        /* CONSTANT OMEGA */
        /******************/

        Constant_Omega::Constant_Omega(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry) :
            ModeTemp<geometry::eulerian4C::Vertical>(name, description, geometry)
        {
          _omega = new Parameter("omega", "The fix value of omega.",
                                 0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_omega);
        }

        Constant_Omega::~Constant_Omega(void)
        {
          delete _omega;
        }

        void
        Constant_Omega::computeAngles(Value const & h, Value const & k, Value const & l,
                                      smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              // La définition de omega dans ce mode.
              double omega = _omega->get_current().get_value();

              // calcule de Chi.
              double s_chi = hphi.y();
              double c_chi = (hphi.x()*hphi.x() + hphi.z()*hphi.z())*cos(omega-theta)*cos(omega-theta)-hphi.y()*hphi.y()*sin(omega-theta)*sin(omega-theta);
              if (c_chi < 0.)
                HKLEXCEPTION("Unreachable reflection.", "Change h k l values");
              else
                c_chi = sqrt(c_chi);
              double chi = convenience::atan2(s_chi, c_chi);

              // Calcule de Phi
              double s_phi = hphi.x()*cos(chi)*cos(omega - theta) - hphi.z()*sin(omega - theta);
              double c_phi = hphi.x()*sin(omega - theta) + hphi.z()*cos(chi)*cos(omega - theta);
              double phi = convenience::atan2(s_phi, c_phi);

              _geometry.omega()->set_current(omega);
              _geometry.chi()->set_current(chi);
              _geometry.phi()->set_current(phi);
              _geometry.tth()->set_current(2.*theta);
            }
        }

        /****************/
        /* CONSTANT CHI */
        /****************/

        Constant_Chi::Constant_Chi(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry) :
            ModeTemp<geometry::eulerian4C::Vertical>(name, description, geometry)
        {
          _chi = new Parameter("chi", "The fix value of chi.",
                               0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_chi);
        }

        Constant_Chi::~Constant_Chi(void)
        {
          delete _chi;
        }

        void
        Constant_Chi::computeAngles(Value const & h, Value const & k, Value const & l,
                                    smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              // La définition de chi dans ce mode.
              double chi = _chi->get_current().get_value();
              //! \todo traiter le cas C=0;

              // calcule de Omega.
              double s_omega_theta = (hphi.x()*hphi.x() + hphi.z()*hphi.z())*sin(chi)*sin(chi) - hphi.y()*hphi.y()*cos(chi)*cos(chi);
              double c_omega_theta = hphi.y();
              if (s_omega_theta < 0.)
                HKLEXCEPTION("Unreachable reflection.", "Change h k l values");
              else
                s_omega_theta = sqrt(s_omega_theta);
              double omega = convenience::atan2(s_omega_theta, c_omega_theta) + theta;

              // Calcule de Phi
              double s_phi = hphi.x()*cos(chi)*cos(omega - theta) - hphi.z()*sin(omega - theta);
              double c_phi = hphi.x()*sin(omega - theta) + hphi.z()*cos(chi)*cos(omega - theta);
              double phi = convenience::atan2(s_phi, c_phi);

              _geometry.omega()->set_current(omega);
              _geometry.chi()->set_current(chi);
              _geometry.phi()->set_current(phi);
              _geometry.tth()->set_current(2.*theta);
            }
        }

        /****************/
        /* CONSTANT PHI */
        /****************/

        Constant_Phi::Constant_Phi(MyString const & name, MyString const & description, geometry::eulerian4C::Vertical & geometry) :
            ModeTemp<geometry::eulerian4C::Vertical>(name, description, geometry)
        {
          _phi = new Parameter("phi", "The fix value of phi.",
                               0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_phi);
        }

        Constant_Phi::~Constant_Phi(void)
        {
          delete _phi;
        }

        void
        Constant_Phi::computeAngles(Value const & h, Value const & k, Value const & l,
                                    smatrix const & UB) const throw (HKLException)
        {
          if (_parametersAreOk(h, k, l, UB))
            {
              double theta;
              svector hphi;
              _computeThetaAndHphi(h, k, l, UB, theta, hphi);

              // La définition de chi dans ce mode.
              double phi = _phi->get_current().get_value();

              // calcule de Omega.
              double s_omega_theta = hphi.x()*cos(phi)-hphi.z()*sin(phi);
              double c_omega_theta = hphi.x()*hphi.x()*sin(phi)*sin(phi)+hphi.y()*hphi.y()+hphi.z()*hphi.z()*cos(phi)*cos(phi)+hphi.x()*hphi.z()*cos(phi)*sin(phi);
              if (c_omega_theta < 0.)
                HKLEXCEPTION("Unreachable reflection.", "Change h k l values");
              else
                c_omega_theta = sqrt(c_omega_theta);
              double omega = convenience::atan2(s_omega_theta, c_omega_theta) + theta;

              // Calcule de Chi
              double s_chi = hphi.y();
              double c_chi = hphi.x()*sin(phi) + hphi.z()*cos(phi);
              double chi = convenience::atan2(s_chi, c_chi);

              _geometry.omega()->set_current(omega);
              _geometry.chi()->set_current(chi);
              _geometry.phi()->set_current(phi);
              _geometry.tth()->set_current(2.*theta);
            }
        }

      } // namespace vertical
    } // namespace eulerian4C
  } // namespace mode
} // namespace hkl
