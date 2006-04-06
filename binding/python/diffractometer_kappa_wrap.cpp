#include "diffractometer_kappa_wrap.h"
#include "constants.h"

Diffractometer_Kappa_wrap::Diffractometer_Kappa_wrap(double alpha)
: Diffractometer_wrap()
{
    addParameter("alpha");
    setParameterValue("alpha", alpha * constant::math::degToRad);
}
