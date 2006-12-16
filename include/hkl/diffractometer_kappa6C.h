#ifndef _DIFFRACTOMETER_KAPPA6C_H_
#define _DIFFRACTOMETER_KAPPA6C_H_

#include "diffractometer.h"
#include "geometry_kappa6C.h"

namespace hkl
{
namespace diffractometer
{

/**
 *  @brief This class describes a four-circle Kappa diffractometer.
 * 
 * The 6C Kappa diffractometer can be seen as a 4C eulerian one provided that we use some formula from the
 * MHATT-CAT, Advanced Photon Source, Argonne National Laboratory (
 * <A HREF="http://www.mhatt.aps.anl.gov/~walko/kappa.pdf">MHATT-CATs Newport Kappa Diffractometer</A>
 * written by Donald A. Walko). Other interesting documentation can be found at the 
 * <A HREF="http://www.px.nsls.bnl.gov/kappa.html">Brookhaven National Laboratory</A>
 */
class Kappa6C : public DiffractometerTemp<geometry::Kappa6C>
{
public:

    /**
     * @brief Default constructor
     */
    Kappa6C(void);

    /**
     * @brief Destructor
     *
     * Destructor
     */
    virtual ~Kappa6C(void);
};

} // namespace diffractometer
} // namespace hkl

#endif // _DIFFRACTOMETER_KAPPA6C_H_
