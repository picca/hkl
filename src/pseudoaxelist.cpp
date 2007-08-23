
#include "pseudoaxelist.h"
#include "pseudoaxe.h"

namespace hkl {

void PseudoAxeList::push_back(hkl::PseudoAxe * pseudoAxe) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00031B02
      PseudoAxeList::iterator iter = _pseudoAxes.begin();
      PseudoAxeList::iterator end = _pseudoAxes.end();
      while( iter != end )
        {
          if ((*iter)->get_name() == pseudoAxe->get_name())
            HKLEXCEPTION("Can not add two times the same pseudoAxe", "Change the name of the axe.");
          ++iter;
        }
      _pseudoAxes.push_back(pseudoAxe);
  // Bouml preserved body end 00031B02
}

PseudoAxeList::iterator PseudoAxeList::begin() 
{
  // Bouml preserved body begin 00031B82
      return _pseudoAxes.begin();
  // Bouml preserved body end 00031B82
}

PseudoAxeList::iterator PseudoAxeList::end() 
{
  // Bouml preserved body begin 00031C02
      return _pseudoAxes.end();
  // Bouml preserved body end 00031C02
}

PseudoAxeList::const_iterator PseudoAxeList::begin() const 
{
  // Bouml preserved body begin 00031C82
      return _pseudoAxes.begin();
  // Bouml preserved body end 00031C82
}

PseudoAxeList::const_iterator PseudoAxeList::end() const 
{
  // Bouml preserved body begin 00031D02
      return _pseudoAxes.end();
  // Bouml preserved body end 00031D02
}

/**
 * @brief Get all the names of the PseudoAxes in the PseudoAxeList
 */
std::vector<std::string> PseudoAxeList::get_names() const 
{
  // Bouml preserved body begin 00033902
      std::vector<std::string> names;
      PseudoAxeList::const_iterator iter = _pseudoAxes.begin();
      PseudoAxeList::const_iterator end = _pseudoAxes.end();
      while(iter != end)
      {
        names.push_back((*iter)->get_name());
        ++iter;
      }
      
      return names;
  // Bouml preserved body end 00033902
}

/**
 * @brief Get an element of the PseudoAxeList.
 * @param name The name of the PseudoAxe to find.
 * @return A pointer on the PseudoAxe or NULL if the pseudoAxe is not present in the PseudoAxeList
 */
hkl::PseudoAxe * PseudoAxeList::operator[](const std::string & name) 
{
  // Bouml preserved body begin 00033882
      PseudoAxeList::iterator iter = _pseudoAxes.begin();
      PseudoAxeList::iterator end = _pseudoAxes.end();
      while(iter != end)
      {
        if ((*iter)->get_name() == name)
          return *iter;
        ++iter;
      }
      return NULL;
  // Bouml preserved body end 00033882
}

/**
 * @brief Get the size of the PseudoAxeList.
 * @return the number of element in the PseudoAxeList.
 */
unsigned int PseudoAxeList::size() const 
{
  // Bouml preserved body begin 00038902
      return _pseudoAxes.size();
  // Bouml preserved body end 00038902
}

void PseudoAxeList::clear() 
{
  // Bouml preserved body begin 00031D82
      _pseudoAxes.clear();
  // Bouml preserved body end 00031D82
}

void PseudoAxeList::set_write_from_read() 
{
  // Bouml preserved body begin 00038382
      iterator iter = _pseudoAxes.begin();
      iterator end = _pseudoAxes.end();
      while(iter != end)
        {
          (*iter)->set_write_from_read();
          ++iter;
        }
  // Bouml preserved body end 00038382
}

/*!
 * \brief print the PseudoAxeList into a flux
 * \param flux The stream to print into.
 */
std::ostream & PseudoAxeList::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00038882
      const_iterator iter = _pseudoAxes.begin();
      const_iterator end = _pseudoAxes.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00038882
}


} // namespace hkl
