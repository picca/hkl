
#include "pseudoaxeenginelist.h"
#include "pseudoaxeengine.h"

namespace hkl
  {

  hkl::PseudoAxeList & PseudoAxeEngineList::pseudoAxes()
  {
    return _pseudoAxes;
  }

  void PseudoAxeEngineList::push_back(hkl::PseudoAxeEngine * pseudoAxeEngine)
  {
    _pseudoAxeEngines.push_back(pseudoAxeEngine);

    PseudoAxeList & pseudoAxes = pseudoAxeEngine->pseudoAxes();
    PseudoAxeList::iterator iter = pseudoAxes.begin();
    PseudoAxeList::iterator end = pseudoAxes.end();
    while (iter != end)
      {
        _pseudoAxes.push_back(*iter);
        ++iter;
      }
  }

  void PseudoAxeEngineList::clear()
  {
    std::vector<PseudoAxeEngine *>::iterator iter = _pseudoAxeEngines.begin();
    std::vector<PseudoAxeEngine *>::iterator end = _pseudoAxeEngines.end();
    while (iter != end)
      {
        delete *iter;
        ++iter;
      }
    _pseudoAxeEngines.clear();
    _pseudoAxes.clear();
  }

  /**
   * \brief Are two PseudoAxeEngineList equals ?
   * \param pseudoAxeEngineList the hkl::PseudoAxeEngineList to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool PseudoAxeEngineList::operator==(const hkl::PseudoAxeEngineList & pseudoAxeEngineList) const
    {
      if (_pseudoAxeEngines.size() != pseudoAxeEngineList._pseudoAxeEngines.size())
        return false;
      else
        {
          std::vector<PseudoAxeEngine *>::const_iterator iter = _pseudoAxeEngines.begin();
          std::vector<PseudoAxeEngine *>::const_iterator end = _pseudoAxeEngines.end();
          std::vector<PseudoAxeEngine *>::const_iterator iter2 = pseudoAxeEngineList._pseudoAxeEngines.begin();
          while (iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
    }

  /**
   * @brief print the PseudoAxeEngineList into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & PseudoAxeEngineList::printToStream(std::ostream & flux) const
    {
      flux << " PseudoAxeEngineList : " << _pseudoAxeEngines.size() << std::endl;
      std::vector<PseudoAxeEngine *>::const_iterator iter = _pseudoAxeEngines.begin();
      std::vector<PseudoAxeEngine *>::const_iterator end = _pseudoAxeEngines.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

} // namespace hkl
