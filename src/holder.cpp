
#include "holder.h"
#include "axe.h"
#include "quaternion.h"

namespace hkl {

/**
 * @brief construct an Holder related to an AxeList.
 */
Holder::Holder(hkl::AxeList & axeList) :
  _axeList(axeList)
{
  // Bouml preserved body begin 0003BC82
  // Bouml preserved body end 0003BC82
}

/**
 * @brief Add an axe to the holder.
 * @return The added axe.
 */
hkl::Axe * Holder::add(hkl::Axe * axe) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003B802
  std::string const & name = axe->get_name();

  // Is the axe in the axeList ?
  hkl::AxeList::iterator iter = _axeList.begin();
  hkl::AxeList::iterator end = _axeList.end();
  bool found_in_axeList = false;
  unsigned int idx = 0;
  while(iter != end || !found_in_axeList )
  {
    if ( (*iter)->get_name() == name) // same name -> check if axes are compatible
    {
      if ( **iter == *axe) // same axe -> check if axe in the holder ( in _axes)
      {
        std::vector<hkl::HolderRow>::iterator it = _axes.begin();
        std::vector<hkl::HolderRow>::iterator it_end = _axes.end();
        while(it != it_end)
        {
          if ( it->axe->get_name() == name) // yes -> exception
          {
              std::ostringstream description;
              description << "The axe \"" << name << "\" is already present in the holder";
              HKLEXCEPTION("Can not add two times the same axe",
                           description.str());
          }
          else // no -> add it
            ++it;
        }
        // not in the holder -> add it and check for memory leak
        hkl::HolderRow row = {NULL, idx};
        if (*iter == axe) // same pointer -> only add to the _axes.
          row.axe = axe;
        else // different pointer -> keep the one from the holder.
          row.axe = *iter;
        _axes.push_back(row);
        return row.axe;
      }
      else // different axe with the same name -> throw exception
      {
        std::ostringstream description;
        description << "Same name but different axe." << endl
          << "holder axe : " << **iter;
        description << "Axe to add : " << *axe;
        HKLEXCEPTION("Can not add this Axe to the sample axe list",
            description.str());
      }
    }
    else // not same name -> next axe in the axeList
    {
      ++idx; // compute the index of the next axe in the _axeList.
      ++iter;
    }
  }
  // Axe not present in the axeList so add it to the axeList and the _axes.
  hkl::HolderRow row = { axe, _axeList.size() };
  _axeList.push_back(axe);
  _axes.push_back(row);
  return row.axe;
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
  std::vector<hkl::HolderRow>::const_iterator iter = _axes.begin();
  std::vector<hkl::HolderRow>::const_iterator end = _axes.begin();
  while (iter != end)
  {
    iter->axe->apply(q);
    ++iter;
  }

  return q;
  // Bouml preserved body end 0003BE02
}

/**
 * @brief set the axeList of the Holder.
 * @param axeList The hkl::AxeList to set.
 * @throw HKLException if the hkl::AxeList is not compatible.
 */
void Holder::set_axeList(hkl::AxeList & axeList) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003BD82
  // Bouml preserved body end 0003BD82
}

/**
 * @brief print the Holder into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & Holder::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003C082
      flux << "  holder: (" << _axes.size() << ")" << endl;
      std::vector<hkl::HolderRow>::const_iterator iter = _axes.begin();
      std::vector<hkl::HolderRow>::const_iterator end = _axes.end();
      while(iter != end)
        {
          flux << *(iter->axe);
          ++iter;
        }
      
      return flux;
  // Bouml preserved body end 0003C082
}


} // namespace hkl
