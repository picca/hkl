/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "eulerian4C_vertical_mode.h"
#include "value.h"
#include "svector.h"
#include "parameter.h"

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      namespace mode
        {

        Bissector::Bissector(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
            ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)
        {
        }

        Bissector::~Bissector()
        {
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
          }

        Delta_Theta::Delta_Theta(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
            ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)
        {
          _dtheta = new Parameter("delta theta", "The omega offset relatively to theta.",
                                  0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_dtheta);
        }

        Delta_Theta::~Delta_Theta()
        {
          delete _dtheta;
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
          }

        Constant_Omega::Constant_Omega(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
            ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)
        {
          _omega = new Parameter("omega", "The fix value of omega.",
                                 0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_omega);
        }

        Constant_Omega::~Constant_Omega()
        {
          delete _omega;
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
          }

        Constant_Chi::Constant_Chi(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
            ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)
        {
          _chi = new Parameter("chi", "The fix value of chi.",
                               0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_chi);
        }

        Constant_Chi::~Constant_Chi()
        {
          delete _chi;
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
          }

        Constant_Phi::Constant_Phi(const std::string & name, const std::string & description, hkl::eulerian4C::vertical::Geometry & geometry) :
            ModeTemp<hkl::eulerian4C::vertical::Geometry>(name, description, geometry)
        {
          _phi = new Parameter("phi", "The fix value of phi.",
                               0 * constant::math::degToRad, 0 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_phi);
        }

        Constant_Phi::~Constant_Phi()
        {
          delete _phi;
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
          }


      } // namespace hkl::eulerian4C::vertical::mode

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
