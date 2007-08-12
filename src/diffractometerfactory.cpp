
#include "diffractometerfactory.h"
#include "diffractometer.h"

namespace hkl {

DiffractometerFactory::DiffractometerFactory() 
{
  // Bouml preserved body begin 00037E82
  // Bouml preserved body end 00037E82
}

DiffractometerFactory::~DiffractometerFactory() 
{
  // Bouml preserved body begin 00037F02
  // Bouml preserved body end 00037F02
}

/**
 * @brief Create a new reflection.
 * @param type The hkl::DiffractometerType of the Diffractometer to create.
 * @param parameter A double use to build the Diffractometer.
 * @return The created Diffractometer.
 *
 * This parameter has no effect for an Eulerian diffractometer.
 * But correspond to the alpha angle of the Kappa Geometry for the Kappa diffractometers.
 */

hkl::Diffractometer * DiffractometerFactory::create(hkl::DiffractometerType type, double parameter) 
{
  // Bouml preserved body begin 00037F82
      Diffractometer * diffractometer;
      
      switch (type)
        {
        case DIFFRACTOMETER_TWOC_VERTICAL :
          diffractometer = new hkl::twoC::vertical::Diffractometer;
          break;
        case DIFFRACTOMETER_EULERIAN4C_VERTICAL :
          diffractometer = new hkl::eulerian4C::vertical::Diffractometer;
          break;
        case DIFFRACTOMETER_EULERIAN6C :
          diffractometer = new hkl::eulerian6C::Diffractometer;
          break;
        case DIFFRACTOMETER_KAPPA4C_VERTICAL :
          diffractometer = new hkl::kappa4C::vertical::Diffractometer(parameter);
          break;
        case DIFFRACTOMETER_KAPPA6C :
          diffractometer = new hkl::kappa6C::Diffractometer(parameter);
          break;
        default :
          HKLEXCEPTION("Unknown diffractometer Type.", "Please use a correct type.");
        }
      return diffractometer;
  // Bouml preserved body end 00037F82
}


} // namespace hkl
