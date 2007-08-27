
#include "holder.h"
#include "axe.h"
#include "axe_rotation.h"
#include "svector.h"
#include "quaternion.h"

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
      _holders[i]->set_holderList(this);
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
    hkl::Holder * holder = new hkl::Holder(this);
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
std::ostream & HolderList::printToStream(std::ostream & flux) const 
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
std::ostream & HolderList::toStream(std::ostream & flux) const 
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
std::istream & HolderList::fromStream(std::istream & flux) 
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

/**
 * @brief construct an Holder related to an AxeList.
 */
Holder::Holder(hkl::HolderList * holderList) :
  _holderList(holderList) 
{
  // Bouml preserved body begin 0003BC82
  // Bouml preserved body end 0003BC82
}

/**
 * @brief Add an axe to the holder.
 * @param name The name of the added Axe.
 * @param axe The hkl::svector representing the axe of rotation.
 * @return The added axe.
 */
hkl::axe::Rotation * Holder::add_rotation(const std::string & name, const hkl::svector & axe) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003B802
  return this->add<hkl::axe::Rotation>(new hkl::axe::Rotation(name, "rotation", -hkl::constant::math::pi, 0, hkl::constant::math::pi, axe));
  // Bouml preserved body end 0003B802
}

/**
 * @brief apply the holder transformation to a hkl::Quaternion.
 * @return The q hkl::Quaternion after the transformation.
 * 
 * It computes q * qi in the Holder.
 */
hkl::Quaternion & Holder::apply(hkl::Quaternion & q) const 
{
  // Bouml preserved body begin 0003BE02
  std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
  std::vector<hkl::HolderRow>::const_iterator end = _rows.end();
  while (iter != end)
  {
    iter->axe->apply(q);
    ++iter;
  }

  return q;
  // Bouml preserved body end 0003BE02
}

/**
 * @brief apply the holder consign transformation to a hkl::Quaternion.
 * @return The q hkl::Quaternion after the transformation.
 * 
 * It computes q * qi(consign) in the Holder.
 */
hkl::Quaternion & Holder::apply_consign(hkl::Quaternion & q) const 
{
  // Bouml preserved body begin 0003F982
    std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
    std::vector<hkl::HolderRow>::const_iterator end = _rows.end();
    while (iter != end)
    {
      iter->axe->apply_consign(q);
      ++iter;
    }
  
    return q;
  // Bouml preserved body end 0003F982
}

/**
 * @brief set the axeList of the Holder.
 * @param holderList The hkl::HolderList to set.
 * @throw HKLException if the hkl::HolderList is not compatible.
 */
void Holder::set_holderList(hkl::HolderList * holderList) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003BD82
  _holderList = holderList;
  std::vector<hkl::HolderRow>::iterator iter = _rows.begin();
  std::vector<hkl::HolderRow>::iterator end = _rows.end();
  while(iter != end)
  {
    iter->axe = _holderList->axes()[iter->idx];
    ++iter;
  }
  // Bouml preserved body end 0003BD82
}

/**
 * @brief Are two Holder equals ?
 * @param holder the hkl::Holder to compare with.
 */
bool Holder::operator==(const hkl::Holder & holder) const 
{
  // Bouml preserved body begin 0003D082
    if (_rows.size() == holder._rows.size())
      {
        std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
        std::vector<hkl::HolderRow>::const_iterator iter2 = holder._rows.begin();
        std::vector<hkl::HolderRow>::const_iterator end = _rows.end();
        while(iter != end)
          {
            if ( iter->idx != iter2->idx)
                return false;
            ++iter;
            ++iter2;
          }
        return true;
      }
  return false;
  // Bouml preserved body end 0003D082
}

/**
 * @brief print the Holder into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & Holder::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0003C082
  flux << "holder: (" << _rows.size() << ") Axe List related : " << &_holderList->axes() << std::endl;
  std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
  std::vector<hkl::HolderRow>::const_iterator end = _rows.end();
  while(iter != end)
  {
    flux << "  (" << iter->axe << ", " << iter->idx << ") "
         << *(iter->axe) << std::endl;
    ++iter;
  }

  return flux;
  // Bouml preserved body end 0003C082
}

/**
 * @brief print on a stream the content of the Holder
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Holder::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0003CE82
  unsigned int size = _rows.size();
  flux << " " << size;
  for(unsigned int i=0;i<size;i++)
    flux << " " << _rows[i].idx;
  flux << std::endl;
  return flux;
  // Bouml preserved body end 0003CE82
}

/**
 * @brief restore the content of the Holder from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Holder::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 0003CF02
  // read the size of the _row when the holder was save.
  unsigned int size;
  flux >> size;
  // check if size is compatible with the size of the actual holder.
  _rows.clear();
  for(unsigned int i=0;i<size;i++)
  {
    unsigned int idx;
    flux >> idx;
    // now update the row in the Axe Row
    hkl::HolderRow row = {_holderList->axes()[idx], idx};
    _rows.push_back(row);
  }
  return flux;
  // Bouml preserved body end 0003CF02
}


} // namespace hkl
