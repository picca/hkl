
#include "pseudoaxeenginelist.h"
#include "pseudoaxeengine.h"

namespace hkl {

hkl::PseudoAxeList & PseudoAxeEngineList::pseudoAxes() 
{
  // Bouml preserved body begin 00031902
      return _pseudoAxes;
  // Bouml preserved body end 00031902
}

void PseudoAxeEngineList::push_back(hkl::PseudoAxeEngine * pseudoAxeEngine) 
{
  // Bouml preserved body begin 00031982
      _pseudoAxeEngines.push_back(pseudoAxeEngine);
      
      PseudoAxeList & pseudoAxes = pseudoAxeEngine->pseudoAxes();
      PseudoAxeList::iterator iter = pseudoAxes.begin();
      PseudoAxeList::iterator end = pseudoAxes.end();
      while(iter != end)
        {
          _pseudoAxes.push_back(*iter);
          ++iter;
        }
  // Bouml preserved body end 00031982
}

void PseudoAxeEngineList::clear() 
{
  // Bouml preserved body begin 00031A82
      vector<PseudoAxeEngine *>::iterator iter = _pseudoAxeEngines.begin();
      vector<PseudoAxeEngine *>::iterator end = _pseudoAxeEngines.end();
      while(iter != end)
        {
          delete *iter;
          ++iter;
        }
      _pseudoAxeEngines.clear();
      _pseudoAxes.clear();
  // Bouml preserved body end 00031A82
}

/**
 * \brief Are two PseudoAxeEngineList equals ?
 * \param pseudoAxeEngineList the hkl::PseudoAxeEngineList to compare with.
 * \return true if both are equals flase otherwise.
 */
bool PseudoAxeEngineList::operator==(const hkl::PseudoAxeEngineList & pseudoAxeEngineList) const 
{
  // Bouml preserved body begin 00031682
      if (_pseudoAxeEngines.size() != pseudoAxeEngineList._pseudoAxeEngines.size())
        return false;
      else
        {
          vector<PseudoAxeEngine *>::const_iterator iter = _pseudoAxeEngines.begin();
          vector<PseudoAxeEngine *>::const_iterator end = _pseudoAxeEngines.end();
          vector<PseudoAxeEngine *>::const_iterator iter2 = pseudoAxeEngineList._pseudoAxeEngines.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
  // Bouml preserved body end 00031682
}

/**
 * @brief print the PseudoAxeEngineList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & PseudoAxeEngineList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00031702
      flux << " PseudoAxeEngineList : " << _pseudoAxeEngines.size() << endl;
      vector<PseudoAxeEngine *>::const_iterator iter = _pseudoAxeEngines.begin();
      vector<PseudoAxeEngine *>::const_iterator end = _pseudoAxeEngines.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00031702
}

/**
 * @brief print on a stream the content of the PseudoAxeEngineList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & PseudoAxeEngineList::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00031782
      flux << " " << _pseudoAxeEngines.size();
      vector<PseudoAxeEngine *>::const_iterator iter = _pseudoAxeEngines.begin();
      vector<PseudoAxeEngine *>::const_iterator end = _pseudoAxeEngines.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00031782
}

/**
 * @brief restore the content of the PseudoAxeEngineList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & PseudoAxeEngineList::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00031802
      unsigned int size;
      flux >> size;
      vector<PseudoAxeEngine *>::iterator iter = _pseudoAxeEngines.begin();
      for(unsigned int i=0;i<size; i++)
        {
          (*iter)->fromStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00031802
}


} // namespace hkl
