#include "diffractometerfactory.h"
#include "diffractometer_twoC.h"
#include "diffractometer_eulerian4C.h"
#include "diffractometer_eulerian6C.h"
#include "diffractometer_kappa4C.h"
#include "diffractometer_kappa6C.h"

using namespace std;

namespace hkl
  {

  DiffractometerFactory::DiffractometerFactory(void)
  {}

  DiffractometerFactory::~DiffractometerFactory(void)
  {}

  Diffractometer *
  DiffractometerFactory::create(DiffractometerType const & type, double parameter) throw (HKLException)
  {
    Diffractometer * diffractometer;

    switch (type)
      {
      case DIFFRACTOMETER_TWOC_VERTICAL :
        diffractometer = new diffractometer::twoC::Vertical;
        break;
      case DIFFRACTOMETER_EULERIAN4C_VERTICAL :
        diffractometer = new diffractometer::eulerian4C::Vertical;
        break;
      case DIFFRACTOMETER_EULERIAN6C :
        diffractometer = new diffractometer::Eulerian6C;
        break;
      case DIFFRACTOMETER_KAPPA4C_VERTICAL :
        diffractometer = new diffractometer::kappa4C::Vertical(parameter);
        break;
      case DIFFRACTOMETER_KAPPA6C :
        diffractometer = new diffractometer::Kappa6C(parameter);
        break;
      default :
        HKLEXCEPTION("Unknown diffractometer Type.", "Please use a correct type.");
      }
    return diffractometer;
  }
} // namespace hkl
