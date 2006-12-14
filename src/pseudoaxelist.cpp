#include "pseudoaxelist.h"

using namespace std;

namespace hkl
  {

  PseudoAxeList::~PseudoAxeList(void)
  {
    _pseudoAxes.clear();
  }

  void
  PseudoAxeList::add(PseudoAxe * pseudoAxe) throw (HKLException)
    {
      _pseudoAxes.push_back(pseudoAxe);
    }

  vector<PseudoAxe *>::iterator
  PseudoAxeList::begin(void)
  {
    return _pseudoAxes.begin();
  }

  vector<PseudoAxe *>::iterator
  PseudoAxeList::end(void)
  {
    return _pseudoAxes.end();
  }

  void
  PseudoAxeList::erase(vector<PseudoAxe *>::iterator pos) throw (HKLException)
  {
    delete *pos;
    _pseudoAxes.erase(pos);
  }

  void
  PseudoAxeList::clear(void)
  {
    vector<PseudoAxe *>::iterator iter = _pseudoAxes.begin();
    vector<PseudoAxe *>::iterator end = _pseudoAxes.end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    _pseudoAxes.clear();
  }

  unsigned int
  PseudoAxeList::size(void) const
    {
      return _pseudoAxes.size();
    }

  PseudoAxe *
  PseudoAxeList::operator[](MyString const & name) throw (HKLException)
  {
    vector<PseudoAxe *>::iterator iter = _pseudoAxes.begin();
    vector<PseudoAxe *>::iterator end = _pseudoAxes.end();
    while(iter != end)
      {
        if ( (*iter)->get_name() == name )
          return *iter;
        ++iter;
      }
    ostringstream reason;
    ostringstream description;
    reason << "The PseudoAxe named \"" << name << "\" does not exist.";

    if (_pseudoAxes.size())
      {
        description << "Available pseudoAxes are:";

        iter = _pseudoAxes.begin();
        while (iter != end)
          {
            description << "\"" << (*iter)->get_name() << "\" ";
            ++iter;
          }
      }
    else
      description << "No pseudoAxe available.";
    HKLEXCEPTION(reason.str(),
                 description.str());
  }

  bool
  PseudoAxeList::operator ==(PseudoAxeList const & pseudoAxeList) const
    {
      if (size() != pseudoAxeList.size())
        return false;
      else
        {
          vector<PseudoAxe *>::const_iterator iter = _pseudoAxes.begin();
          vector<PseudoAxe *>::const_iterator end = _pseudoAxes.end();
          vector<PseudoAxe *>::const_iterator iter2 = pseudoAxeList._pseudoAxes.begin();
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
  PseudoAxeList::printToStream(ostream & flux) const
    {
      flux << " PseudoAxeList : " << _pseudoAxes.size() << endl;
      vector<PseudoAxe *>::const_iterator iter = _pseudoAxes.begin();
      vector<PseudoAxe *>::const_iterator end = _pseudoAxes.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  PseudoAxeList::toStream(ostream & flux) const
    {
      flux << " " << _pseudoAxes.size();
      vector<PseudoAxe *>::const_iterator iter = _pseudoAxes.begin();
      vector<PseudoAxe *>::const_iterator end = _pseudoAxes.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  PseudoAxeList::fromStream(istream & flux)
  {
    unsigned int size;
    int type;
    flux >> size;
    vector<PseudoAxe *>::iterator iter = _pseudoAxes.begin();
    for(unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

} // namespace hkl
