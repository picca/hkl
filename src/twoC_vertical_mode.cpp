
#include "twoC_vertical_mode.h"
#include "value.h"
#include "svector.h"

namespace hkl
  {

  namespace twoC
    {

    namespace vertical
      {

      namespace mode
        {

        Symetric::Symetric(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry) :
            ModeTemp<hkl::twoC::vertical::Geometry>(name, description, geometry)
        {
        }

        Symetric::~Symetric()
        {
        }

        /**
         * @brief The main function to get a sample of angles from (h,k,l).
         * @param h The scaterring vector first coordinate.
         * @param k The scaterring vector second coordinate.
         * @param l The scaterring vector third coordinate.
         * @param UB The product of the orientation matrix U by the crystal matrix B.
         */

        void Symetric::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const
          {
            if (this->_parametersAreOk(h, k, l, UB))
              {
                double theta;
                svector hphi;
                this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);

                _geometry.omega()->set_consign(theta);
                _geometry.tth()->set_consign(2.*theta);
              }
          }

        Fix_Incidence::Fix_Incidence(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry) :
            ModeTemp<hkl::twoC::vertical::Geometry>(name, description, geometry)
        {
        }

        Fix_Incidence::~Fix_Incidence()
        {
        }

        /**
         * @brief The main function to get a sample of angles from (h,k,l).
         * @param h The scaterring vector first coordinate.
         * @param k The scaterring vector second coordinate.
         * @param l The scaterring vector third coordinate.
         * @param UB The product of the orientation matrix U by the crystal matrix B.
         */

        void Fix_Incidence::computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const
          {
            if (this->_parametersAreOk(h, k, l, UB))
              {
                double theta;
                svector hphi;
                this->_computeThetaAndHphi(h, k, l, UB, theta, hphi);

                _geometry.tth()->set_consign(2.*theta);
              }
          }


      } // namespace hkl::twoC::vertical::mode

    } // namespace hkl::twoC::vertical

  } // namespace hkl::twoC

} // namespace hkl
