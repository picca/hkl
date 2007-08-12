
#include "twoC_vertical_mode.h"
#include "value.h"
#include "svector.h"

namespace hkl {

namespace twoC {

namespace vertical {

namespace mode {

Symetric::Symetric(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry) :
  ModeTemp<hkl::twoC::vertical::Geometry>(name, description, geometry) 
{
  // Bouml preserved body begin 00035B82
  // Bouml preserved body end 00035B82
}

Symetric::~Symetric() 
{
  // Bouml preserved body begin 00035C02
  // Bouml preserved body end 00035C02
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
  // Bouml preserved body begin 00035C82
      if (_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          _computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
          _geometry.omega()->set_current(theta);
          _geometry.tth()->set_current(2.*theta);
        }
  // Bouml preserved body end 00035C82
}

Fix_Incidence::Fix_Incidence(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry) :
  ModeTemp<hkl::twoC::vertical::Geometry>(name, description, geometry) 
{
  // Bouml preserved body begin 00035D02
  // Bouml preserved body end 00035D02
}

Fix_Incidence::~Fix_Incidence() 
{
  // Bouml preserved body begin 00035D82
  // Bouml preserved body end 00035D82
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
  // Bouml preserved body begin 00035E02
      if (_parametersAreOk(h, k, l, UB))
        {
          double theta;
          svector hphi;
          _computeThetaAndHphi(h, k, l, UB, theta, hphi);
      
          _geometry.tth()->set_current(2.*theta);
        }
  // Bouml preserved body end 00035E02
}


} // namespace hkl::twoC::vertical::mode

} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
