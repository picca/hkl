
#include "holderlist.h"

namespace hkl {

hkl::AxeFactory HolderList::_axeFactory;

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
  _holders(source._holders)
{
  // Bouml preserved body begin 0003C602
  // need to do a deep copy of the source._axes.
  hkl::AxeList::const_iterator iter = source._axes.begin();
  hkl::AxeList::const_iterator end = source._axes.end();
  while(iter != end)
  {
    _axes.push_back((*iter)->clone());
    ++iter;
  }

  // make a deep copy of the holders and update the axelist due to the deep copy.
  for(unsigned int i=0;i<_holders.size();i++)
  {
    _holders[i] = new hkl::Holder(*_holders[i]);
    _holders[i]->set_axes(&_axes);
  }
  // Bouml preserved body end 0003C602
}

/**
 * @brief add an holder to the HolderList
 * @return The added Holder
 */
hkl::Holder * HolderList::add() 
{
  // Bouml preserved body begin 0003CA82
  hkl::Holder * holder = new hkl::Holder(&_axes);
  _holders.push_back(holder);

  return holder;
  // Bouml preserved body end 0003CA82
}

/**
 * @brief Get the size of the HolderList
 * @return The number of Holder in the HolderList
 */
unsigned int HolderList::size() const 
{
  // Bouml preserved body begin 0003D502
  return _holders.size();
  // Bouml preserved body end 0003D502
}

/**
 * @brief Are two HolderList equals ?
 * @param holderList the hkl::HolderList to compare with.
 */
bool HolderList::operator==(const hkl::HolderList & holderList) const 
{
  // Bouml preserved body begin 0003D582
    if(_axes == holderList._axes)
    {
      if (_holders.size() == holderList._holders.size())
      {
        std::vector<hkl::Holder *>::const_iterator iter = _holders.begin();
        std::vector<hkl::Holder *>::const_iterator iter2 = holderList._holders.begin();
        std::vector<hkl::Holder *>::const_iterator end = _holders.end();
        while(iter != end)
        {
          if ( !(**iter == **iter2) )
            return false;
          ++iter;
          ++iter2;
        }
        return true;
      }
    }
    return false;
  // Bouml preserved body end 0003D582
}

/**
 * @brief print the HolderList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & HolderList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003CD02
  flux << "HolderList with " << _holders.size() << " Holder(s)" << std::endl; 
  flux << "AxeList : " << &_axes << std::endl;
  for(unsigned int i=0;i<_axes.size();++i)
    flux << "  " << _axes[i] << " " << *_axes[i] << std::endl;
  flux << std::endl;
  // now the holders
  for(unsigned int i=0;i<_holders.size();++i)
      flux << "(" << i << ") " << *_holders[i] << std::endl;
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
  // Store the AxeList
  unsigned int size = _axes.size();
  flux << " " << size << std::endl;
  for(unsigned int i=0;i<size;i++)
  {
    flux << " " << _axes[i]->get_type() << std::endl;
    _axes[i]->toStream(flux);
  }

  // now the holders
  size = _holders.size();
  flux << " " << size << std::endl;
  for(unsigned int i=0;i<size;i++)
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
  
  // restaure the AxeList
  // start by clearing the AxeList
  for(unsigned int i=0; i< _axes.size(); i++)
    delete _axes[i];
  _axes.clear();
  // get the number of Axis in the AxeList previously saved.
  unsigned int size;
  flux >> size;
  for(unsigned int i=0;i<size;i++)
  {
    unsigned int type;
    flux >> type;
    hkl::Axe * axe = _axeFactory.create((hkl::AxeType)type, "dummy");
    axe->fromStream(flux);
    _axes.push_back(axe);
  }

  // restore the holders.
  // Start by clearing them.
  for(unsigned int i=0; i<_holders.size();i++)
    delete _holders[i];
  _holders.clear();
  // recreate the holders fill with the right parameters
  flux >> size;
  for(unsigned int i=0;i<size;i++)
  {
    hkl::Holder * holder = this->add();
    holder->fromStream(flux);
  }
  return flux;
  // Bouml preserved body end 0003CE02
}


} // namespace hkl
