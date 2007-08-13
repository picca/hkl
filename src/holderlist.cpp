
#include "holderlist.h"

namespace hkl {

/**
 * @brief Create an empty holderList.
 */
HolderList::HolderList() 
{
  // Bouml preserved body begin 0003C502
  // Bouml preserved body end 0003C502
}

HolderList::~HolderList() 
{
  // Bouml preserved body begin 0003C582
  // remove all axes
  for(unsigned int i=0;i<_axes.size();i++)
    delete _axes[i];
  // remove all holders
  for(unsigned int i=0;i<_holders.size();i++)
    delete _holders[i];
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
    _holders[i]->set_axes(_axes);
  // Bouml preserved body end 0003C602
}

/**
 * @brief add an holder to the HolderList
 * @return The added Holder
 */
hkl::Holder * HolderList::add() 
{
  // Bouml preserved body begin 0003CA82
  hkl::Holder * holder = new hkl::Holder(_axes);
  _holders.push_back(holder);

  return holder;
  // Bouml preserved body end 0003CA82
}

/**
 * @brief print the HolderList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & HolderList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003CD02
  for(unsigned int i=0;i<_holders.size();i++)
    flux << _holders[i];
  return flux;
  // Bouml preserved body end 0003CD02
}

/**
 * @brief print on a stream the content of the HolderList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & HolderList::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003CD82
  for(unsigned int i=0;i<_holders.size();i++)
    _holders[i]->toStream(flux);
  return flux;
  // Bouml preserved body end 0003CD82
}

/**
 * @brief restore the content of the HolderList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & HolderList::fromStream(istream & flux) 
{
  // Bouml preserved body begin 0003CE02
  for(unsigned int i=0;i<_holders.size();i++)
    _holders[i]->fromStream(flux);
  return flux;
  // Bouml preserved body end 0003CE02
}


} // namespace hkl
