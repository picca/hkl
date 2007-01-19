#include "pseudoaxeenginelist.h"

using namespace std;

namespace hkl
  {

  PseudoAxeEngineList::~PseudoAxeEngineList(void)
  {
    //clear();
  }

  void
  PseudoAxeEngineList::add(PseudoAxeEngine * pseudoAxeEngine) throw (HKLException)
    {
      vector<PseudoAxeEngine *>::push_back(pseudoAxeEngine);

      PseudoAxeList & pseudoAxes = pseudoAxeEngine->pseudoAxes();
      PseudoAxeList::iterator iter = pseudoAxes.begin();
      PseudoAxeList::iterator end = pseudoAxes.end();
      while(iter != end)
        {
          _pseudoAxes.push_back(*iter);
          ++iter;
        }
    }

  void
  PseudoAxeEngineList::clear(void)
  {
    vector<PseudoAxeEngine *>::iterator iter = vector<PseudoAxeEngine *>::begin();
    vector<PseudoAxeEngine *>::iterator end = vector<PseudoAxeEngine *>::end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    vector<PseudoAxeEngine *>::clear();
  }

  bool
  PseudoAxeEngineList::operator ==(PseudoAxeEngineList const & pseudoAxeEngineList) const
    {
      if (size() != pseudoAxeEngineList.size())
        return false;
      else
        {
          vector<PseudoAxeEngine *>::const_iterator iter = vector<PseudoAxeEngine *>::begin();
          vector<PseudoAxeEngine *>::const_iterator end = vector<PseudoAxeEngine *>::end();
          vector<PseudoAxeEngine *>::const_iterator iter2 = pseudoAxeEngineList.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
    }

  ostream &
  PseudoAxeEngineList::printToStream(ostream & flux) const
    {
      flux << " PseudoAxeEngineList : " << vector<PseudoAxeEngine *>::size() << endl;
      vector<PseudoAxeEngine *>::const_iterator iter = vector<PseudoAxeEngine *>::begin();
      vector<PseudoAxeEngine *>::const_iterator end = vector<PseudoAxeEngine *>::end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  PseudoAxeEngineList::toStream(ostream & flux) const
    {
      flux << " " << vector<PseudoAxeEngine *>::size();
      vector<PseudoAxeEngine *>::const_iterator iter = vector<PseudoAxeEngine *>::begin();
      vector<PseudoAxeEngine *>::const_iterator end = vector<PseudoAxeEngine *>::end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  PseudoAxeEngineList::fromStream(istream & flux)
  {
    unsigned int size;
    int type;
    flux >> size;
    vector<PseudoAxeEngine *>::iterator iter = vector<PseudoAxeEngine *>::begin();
    for(unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

} // namespace hkl
