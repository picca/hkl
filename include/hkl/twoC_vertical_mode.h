#ifndef _TWOC_VERTICAL_MODE_H
#define _TWOC_VERTICAL_MODE_H


#include "mode.h"
#include <string>
#include "twoC_vertical_geometry.h"

namespace hkl { class Value; } 
namespace hkl { class smatrix; } 

namespace hkl {

namespace twoC {

namespace vertical {

namespace mode {

class Symetric : public hkl::ModeTemp<hkl::twoC::vertical::Geometry> {
  public:
    Symetric(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry);

    virtual ~Symetric();

    /**
     * @brief The main function to get a sample of angles from (h,k,l).
     * @param h The scaterring vector first coordinate.
     * @param k The scaterring vector second coordinate.
     * @param l The scaterring vector third coordinate.
     * @param UB The product of the orientation matrix U by the crystal matrix B.
     */
    
    virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

};
class Fix_Incidence : public hkl::ModeTemp<hkl::twoC::vertical::Geometry> {
  public:
    Fix_Incidence(const std::string & name, const std::string & description, hkl::twoC::vertical::Geometry & geometry);

    virtual ~Fix_Incidence();

    /**
     * @brief The main function to get a sample of angles from (h,k,l).
     * @param h The scaterring vector first coordinate.
     * @param k The scaterring vector second coordinate.
     * @param l The scaterring vector third coordinate.
     * @param UB The product of the orientation matrix U by the crystal matrix B.
     */
    
    virtual void computeAngles(const hkl::Value & h, const hkl::Value & k, const hkl::Value & l, const hkl::smatrix & UB) const;

};

} // namespace hkl::twoC::vertical::mode

} // namespace hkl::twoC::vertical

} // namespace hkl::twoC

} // namespace hkl
#endif
