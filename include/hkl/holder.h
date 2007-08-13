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
    hkl::AxeList & _axes;

    std::vector<hkl::HolderRow> _rows;


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
    void set_axes(hkl::AxeList & axeList) throw(hkl::HKLException);

    /**
     * @brief Are two Holder equals ?
     * @param holder the Holder to compare with.
     */
    bool operator==(const Holder & holder) const;

    /**
     * @brief print the Holder into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the Holder
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the Holder from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};

} // namespace hkl
/*!
 * @brief Overload of the << operator for the Holder class
 * @param flux
 * @param holder
 * @return the modified flux.
 */
inline ostream &
operator << (ostream & flux, hkl::Holder const & holder)
{
  return holder.printToStream(flux);
}

#endif
