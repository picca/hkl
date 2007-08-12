#ifndef _HKL_HOLDER_H
#define _HKL_HOLDER_H


#include <vector>
#include "HKLException.h"
#include <iostream>
using namespace std;

namespace hkl { class Axe; } 
namespace hkl { class AxeList; } 
namespace hkl { class Quaternion; } 

namespace hkl {

struct HolderRow {
    Axe * axe;

    unsigned int idx;

};
class Holder {
  protected:
    hkl::AxeList & _axeList;

    std::vector<hkl::HolderRow> _axes;


  public:
    /**
     * @brief construct an Holder related to an AxeList.
     */
    Holder(hkl::AxeList & axeList);

    /**
     * @brief Add an axe to the holder.
     * @return The added axe.
     */
    hkl::Axe * add(hkl::Axe * axe) throw(hkl::HKLException);

    /**
     * @brief apply the holder transformation to a hkl::Quaternion.
     * @return The q hkl::Quaternion after the transformation.
     * 
     * It computes q * qi in the Holder.
     */
    hkl::Quaternion & apply(hkl::Quaternion & q) const;

    /**
     * @brief set the axeList of the Holder.
     * @param axeList The hkl::AxeList to set.
     * @throw HKLException if the hkl::AxeList is not compatible.
     */
    void set_axeList(hkl::AxeList & axeList) throw(hkl::HKLException);

    /**
     * @brief print the Holder into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

};

} // namespace hkl
#endif
