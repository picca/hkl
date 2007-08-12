#ifndef _KAPPA4C_VERTICAL_DIFFRACTOMETER_H
#define _KAPPA4C_VERTICAL_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "kappa4C_vertical_pseudoaxeengine.h"
#include "kappa4C_vertical_mode.h"
#include "kappa4C_vertical_geometry.h"

namespace hkl {

namespace kappa4C {

namespace vertical {

class Diffractometer : public hkl::DiffractometerTemp<hkl::kappa4C::vertical::Geometry> {
  public:
    Diffractometer(double alpha);

    virtual ~Diffractometer();

};

} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
#endif
