
#include "holder.h"
#include "axe.h"
#include "quaternion.h"

namespace hkl {

/**
 * @brief construct an Holder related to an AxeList.
 */
Holder::Holder(hkl::AxeList * axeList) :
  _axes(axeList) 
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
  hkl::AxeList::iterator iter = _axes->begin();
  hkl::AxeList::iterator end = _axes->end();
  bool found_in_axeList = false;
  unsigned int idx = 0;
  while(iter != end && !found_in_axeList )
  {
    if ( (*iter)->get_name() == name) // same name -> check if axes are compatible
    {
      if ( **iter == *axe) // same axe -> check if axe in the holder ( in _axes)
      {
        std::vector<hkl::HolderRow>::iterator it = _rows.begin();
        std::vector<hkl::HolderRow>::iterator it_end = _rows.end();
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
        _rows.push_back(row);
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
  hkl::HolderRow row = { axe, _axes->size() };
  _axes->push_back(axe);
  _rows.push_back(row);
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
  std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
  std::vector<hkl::HolderRow>::const_iterator end = _rows.begin();
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
void Holder::set_axes(hkl::AxeList * axeList) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003BD82
  _axes = axeList;
  std::vector<hkl::HolderRow>::iterator iter = _rows.begin();
  std::vector<hkl::HolderRow>::iterator end = _rows.end();
  while(iter != end)
  {
    iter->axe = _axes->operator[](iter->idx);
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
  if(*_axes == *holder._axes)
  {
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
  }
  return false;
  // Bouml preserved body end 0003D082
}

/**
 * @brief print the Holder into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & Holder::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003C082
  flux << "holder: (" << _rows.size() << ") Axe List related : " << _axes << std::endl;
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
ostream & Holder::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003CE82
  unsigned int size = _rows.size();
  flux << " " << size << std::endl;
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
istream & Holder::fromStream(istream & flux) 
{
  // Bouml preserved body begin 0003CF02
  // read the size of the _row whene the holder was save.
  unsigned int size;
  flux >> size;
  // check if size is compatible with the size of the actual holder.
  _rows.clear();
  for(unsigned int i=0;i<size;i++)
  {
    unsigned int idx;
    flux >> idx;
    // now update the row in the Axe Row
    hkl::HolderRow row = {_axes->operator[](idx), idx};
    _rows.push_back(row);
  }
  return flux;
  // Bouml preserved body end 0003CF02
}


} // namespace hkl
