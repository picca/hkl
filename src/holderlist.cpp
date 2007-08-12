
#include "holderlist.h"
#include "holder.h"

namespace hkl {

/**
 * @brief Create an holderList with size number of holder in it.
 */
HolderList::HolderList(unsigned int size) 
{
  // Bouml preserved body begin 0003C502
  for(unsigned int i=0;i<size;i++)
    _holders.push_back(hkl::Holder(_axes));
  // Bouml preserved body end 0003C502
}

HolderList::~HolderList() 
{
  // Bouml preserved body begin 0003C582
  for(unsigned int i=0;i<_axes.size();i++)
    delete _axes[i];
  // Bouml preserved body end 0003C582
}

HolderList::HolderList(const hkl::HolderList & source) :
  _axes(source._axes),
  _holders(source._holders)
{
  // Bouml preserved body begin 0003C602
  // need to do a deep copy of the _axes.
  hkl::AxeList::iterator iter = _axes.begin();
  hkl::AxeList::iterator end = _axes.end();
  while(iter != end)
  {
    *iter = (*iter)->clone();
    ++iter;
  }

  // now we must update the holders as the copy do not take into account the deep copy of the _axes.
  for(unsigned int i=0;i<_holders.size();i++)
    _holders[i].set_axeList(_axes);
  // Bouml preserved body end 0003C602
}


} // namespace hkl
