
#include "eulerian4C_vertical_mode.h"
#include "value.h"
#include "svector.h"
#include "parameter.h"

namespace hkl {

namespace eulerian4C {

namespace vertical {

namespace mode {

Bissector::Bissector(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
  ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry) 
{
  // Bouml preserved body begin 00035602
  // Bouml preserved body end 00035602
}

Bissector::~Bissector() 
{
  // Bouml preserved body begin 00035682
  // Bouml preserved body end 00035682
}

/**
 * @brief The main function to get a sample of angles from (h,k,l).
 * @param h The scaterring vector first coordinate.
 * @param k The scaterring vector second coordinate.
 * @param l The scaterring vector third coordinate.
 * @param UB The product of the orientation matrix U by the crystal matrix B.
 */

void Bissector::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const 
{
  // Bouml preserved body begin 00035402
      if (this->_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
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
      
          _geometry.omega()->set_consign(omega);
          _geometry.chi()->set_consign(chi);
          _geometry.phi()->set_consign(phi);
          _geometry.tth()->set_consign(2.*theta);
        }
  // Bouml preserved body end 00035402
}

Delta_Theta::Delta_Theta(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
  ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)  
{
  // Bouml preserved body begin 00035502
      _dtheta = new Parameter("delta theta", "The omega offset relatively to theta.",
                                        0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
      _parameters.add(_dtheta);
  // Bouml preserved body end 00035502
}

Delta_Theta::~Delta_Theta() 
{
  // Bouml preserved body begin 00035582
      delete _dtheta;
  // Bouml preserved body end 00035582
}

/**
 * @brief The main function to get a sample of angles from (h,k,l).
 * @param h The scaterring vector first coordinate.
 * @param k The scaterring vector second coordinate.
 * @param l The scaterring vector third coordinate.
 * @param UB The product of the orientation matrix U by the crystal matrix B.
 */

void Delta_Theta::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const 
{
  // Bouml preserved body begin 00035482
      if (this->_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
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
      
          _geometry.omega()->set_consign(omega);
          _geometry.chi()->set_consign(chi);
          _geometry.phi()->set_consign(phi);
          _geometry.tth()->set_consign(2.*theta);
        }
  // Bouml preserved body end 00035482
}

Constant_Omega::Constant_Omega(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
  ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)  
{
  // Bouml preserved body begin 00035702
      _omega = new Parameter("omega", "The fix value of omega.",
                                       0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
      _parameters.add(_omega);
  // Bouml preserved body end 00035702
}

Constant_Omega::~Constant_Omega() 
{
  // Bouml preserved body begin 00035782
      delete _omega;
  // Bouml preserved body end 00035782
}

/**
 * @brief The main function to get a sample of angles from (h,k,l).
 * @param h The scaterring vector first coordinate.
 * @param k The scaterring vector second coordinate.
 * @param l The scaterring vector third coordinate.
 * @param UB The product of the orientation matrix U by the crystal matrix B.
 */

void Constant_Omega::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const 
{
  // Bouml preserved body begin 00035802
      if (this->_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
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
      
          _geometry.omega()->set_consign(omega);
          _geometry.chi()->set_consign(chi);
          _geometry.phi()->set_consign(phi);
          _geometry.tth()->set_consign(2.*theta);
        }
  // Bouml preserved body end 00035802
}

Constant_Chi::Constant_Chi(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
  ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)  
{
  // Bouml preserved body begin 00035882
      _chi = new Parameter("chi", "The fix value of chi.",
                                     0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
      _parameters.add(_chi);
  // Bouml preserved body end 00035882
}

Constant_Chi::~Constant_Chi() 
{
  // Bouml preserved body begin 00035902
      delete _chi;
  // Bouml preserved body end 00035902
}

/**
 * @brief The main function to get a sample of angles from (h,k,l).
 * @param h The scaterring vector first coordinate.
 * @param k The scaterring vector second coordinate.
 * @param l The scaterring vector third coordinate.
 * @param UB The product of the orientation matrix U by the crystal matrix B.
 */

void Constant_Chi::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const 
{
  // Bouml preserved body begin 00035982
      if (this->_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
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
      
          _geometry.omega()->set_consign(omega);
          _geometry.chi()->set_consign(chi);
          _geometry.phi()->set_consign(phi);
          _geometry.tth()->set_consign(2.*theta);
        }
  // Bouml preserved body end 00035982
}

Constant_Phi::Constant_Phi(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
  ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)  
{
  // Bouml preserved body begin 00035A02
      _phi = new Parameter("phi", "The fix value of phi.",
                                     0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
      _parameters.add(_phi);
  // Bouml preserved body end 00035A02
}

Constant_Phi::~Constant_Phi() 
{
  // Bouml preserved body begin 00035A82
      delete _phi;
  // Bouml preserved body end 00035A82
}

/**
 * @brief The main function to get a sample of angles from (h,k,l).
 * @param h The scaterring vector first coordinate.
 * @param k The scaterring vector second coordinate.
 * @param l The scaterring vector third coordinate.
 * @param UB The product of the orientation matrix U by the crystal matrix B.
 */

void Constant_Phi::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const 
{
  // Bouml preserved body begin 00035B02
      if (this->_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
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
      
          _geometry.omega()->set_consign(omega);
          _geometry.chi()->set_consign(chi);
          _geometry.phi()->set_consign(phi);
          _geometry.tth()->set_consign(2.*theta);
        }
  // Bouml preserved body end 00035B02
}


} // namespace hkl::eulerian4C::vertical::mode

} // namespace hkl::eulerian4C::vertical

} // namespace hkl::eulerian4C

} // namespace hkl
