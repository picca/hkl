#ifndef _HKL_HOLDERLIST_H
#define _HKL_HOLDERLIST_H


#include "axe.h"
#include <vector>
#include "holder.h"
#include <iostream>
using namespace std;

namespace hkl {

class HolderList {
  protected:
    hkl::AxeList _axes;

    std::vector<hkl::Holder *> _holders;


  public:
    /**
     * @brief Create an empty holderList.
     */
    HolderList();

    virtual ~HolderList();

    HolderList(const HolderList & source);

    /**
     * @brief add an holder to the HolderList
     * @return The added Holder
     */
    hkl::Holder * add();

    /**
     * @brief get the axes store in the holderList.
     * @return all axes stored in all holders of the HolderList.
     */
    inline hkl::AxeList & axes();

    /**
     * @brief get the axes store in the holderList.
     * @return all axes stored in all holders of the HolderList.
     */
    inline hkl::AxeList const & axes() const;

    /**
     * @brief get an Holder by its index.
     * @param idx the index of the holder to get.
     */
    inline hkl::Holder const * operator[](unsigned int idx) const;

    /**
     * @brief print the HolderList into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    virtual ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the HolderList
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the HolderList from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};
/**
 * @brief get the axes store in the holderList.
 * @return all axes stored in all holders of the HolderList.
 */
inline hkl::AxeList & HolderList::axes() 
{
  // Bouml preserved body begin 0003CB02
  return _axes;
  // Bouml preserved body end 0003CB02
}

/**
 * @brief get the axes store in the holderList.
 * @return all axes stored in all holders of the HolderList.
 */
inline hkl::AxeList const & HolderList::axes() const 
{
  // Bouml preserved body begin 0003CB82
  return _axes;
  // Bouml preserved body end 0003CB82
}

/**
 * @brief get an Holder by its index.
 * @param idx the index of the holder to get.
 */
inline hkl::Holder const * HolderList::operator[](unsigned int idx) const 
{
  // Bouml preserved body begin 0003CC02
  return _holders[idx];
  // Bouml preserved body end 0003CC02
}


} // namespace hkl
#endif
